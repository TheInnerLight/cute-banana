{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CuteBanana.TH 
  ( bindView
  ) where

import Control.Monad (forM, unless, forM_)
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (find, stripPrefix)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Reactive.Banana (Behavior)
import CuteBanana.Markup
import CuteBanana.Qt.Widget
import CuteBanana.Qt.Widgets (widgets)
import CuteBanana.ViewModel (Command, Field, fieldValue, noCommand, readOnlyField)
import Graphics.UI.Qtah.Widgets.QWidget (QWidget)

bindView :: FilePath -> Name -> Q Exp
bindView path vmType = do
  addDependentFile path
  markup <- runIO (TIO.readFile path)
  root <- either fail pure (parseMarkup path markup)
  vm <- readViewModel vmType
  ws <- mapM readWidget widgets
  tree <- case resolve ws vm root of
    Left (ResolveError (Pos line col) msg) -> fail $ path ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ T.unpack msg
    Right t -> pure t
  generate tree

data ViewModel = ViewModel
  { viewModelName :: String
  , viewModelFields :: [(String, Name, Type)]
  }

data Widget = Widget
  { widgetName :: String
  , widgetCtor :: Name
  , widgetBuild :: Name
  , widgetAttrSlots :: [Slot]
  , widgetChildren :: Maybe Name
  }

data Slot = Slot
  { slotAttr  :: String
  , slotField :: Name
  , slotKind  :: SlotKind
  }

data SlotKind
  = IdSlot              -- ^ The element's id (Maybe Text)
  | ValueSlot AttrSpec  -- ^ Behavior, Field or Command

readViewModel :: Name -> Q ViewModel
readViewModel ty = do
  (_, fields) <- recordFields ty
  pure $ ViewModel
    { viewModelName = nameBase ty
    , viewModelFields = (\(f,t) -> (nameBase f, f, t)) <$> fields
    }

readWidget :: WidgetDef -> Q Widget
readWidget (WidgetDef ty build) = do
  (con, fields) <- recordFields ty
  fields' <- forM fields $ \(field, t) ->
    case (t, attributeName ty field, slotKindOf t) of
      (AppT ListT (ConT q), _, _) | q == ''QWidget -> pure $ Right field
      (_, Just attr, Just kind) -> pure $ Left $ Slot
        { slotAttr = attr
        , slotField = field
        , slotKind = kind 
        }
      _ -> fail $ "widget " ++ nameBase ty ++ " can't use field " ++ nameBase field ++ " :: " ++ pprint t
  let (attrSlots, childFields) = partitionEithers fields'
  pure $ Widget
    { widgetName = nameBase ty
    , widgetCtor = con
    , widgetBuild = build
    , widgetAttrSlots = attrSlots
    , widgetChildren = listToMaybe childFields
    }

recordFields :: Name -> Q (Name, [(Name, Type)])
recordFields ty = do
  info <- reify ty
  case info of
    TyConI (DataD _ _ [] _ [RecC con fs] _) ->
      let fs' = (\(f, _, t) -> (f, t)) <$> fs
      in pure (con, fs')
    _ -> fail $ nameBase ty ++ " must be a record with one constructor and no type parameters"


attributeName :: Name -> Name -> Maybe String
attributeName ty field =
  case stripPrefix (lowerFirstChar (nameBase ty)) (nameBase field) of
    Just rest@(_ : _) -> Just $ lowerFirstChar rest
    _ -> Nothing
  where
    lowerFirstChar (c:cs) = toLower c : cs
    lowerFirstChar [] = []

slotKindOf :: Type -> Maybe SlotKind
slotKindOf t = case t of
  AppT (ConT c) (ConT x) | c == ''Maybe, x == ''T.Text -> Just IdSlot
  AppT (ConT c) a | c == ''Behavior -> ValueSlot . DisplayAttrSpec <$> valueTypeOf a
                  | c == ''Field    -> ValueSlot . EditableAttrSpec <$> valueTypeOf a
  ConT c | c == ''Command -> Just $ ValueSlot ActionAttrSpec
  _ -> Nothing
  where
    valueTypeOf (ConT x) | x == ''T.Text = Just VText
                         | x == ''Bool   = Just VBool
    valueTypeOf _ = Nothing

data Element = Element
  { elementWidget :: Widget
  , elementFields :: [(Name, Value)]
  , elementChildren :: [Element]
  }

data Value
  = ElementId (Maybe String)
  | ViewModelField Name
  | ValueOf Value
  | ReadOnly Value
  | TextConst String
  | BoolConst Bool
  | NoCommand

data ResolveError 
  = ResolveError Pos Text
  deriving (Eq, Show)

resolve :: [Widget] -> ViewModel -> Node -> Either ResolveError Element
resolve ws vm node = do
  w <- case find ((== nodeName node) . widgetName) ws of
    Just w -> Right w
    Nothing -> Left $ ResolveError (nodePos node) $ "unknown element <" <> T.pack (nodeName node) <> ">"

  forM_ (nodeAttrs node) $ \a ->
    unless (attrName a `elem` map slotAttr (widgetAttrSlots w)) $ Left $ ResolveError (attrPos a) $ "<" <> T.pack (widgetName w) <> "> has no attribute '" <> T.pack (attrName a) <> "'"

  values <- forM (widgetAttrSlots w) $ \s -> do
    let attr = (\a -> (attrValuePos a, attrValue a)) <$> find ((== slotAttr s) . attrName) (nodeAttrs node)
    case (attr, slotKind s) of
      (Nothing, IdSlot) -> Right (slotField s, ElementId Nothing)
      (Just (_, Literal s'), IdSlot) -> Right (slotField s, ElementId $ Just s')
      (Just (at, Binding _), IdSlot) -> Left $ ResolveError at "'id' must be a literal"
      (_, ValueSlot spec) -> (slotField s, ) <$> resolveValue vm (slotAttr s) spec attr

  children <- case widgetChildren w of
    Just _  -> mapM (resolve ws vm) (nodeChildren node)
    Nothing -> case nodeChildren node of
      []    -> Right []
      k : _ -> Left $ ResolveError (nodePos k) $ "<" <> T.pack(widgetName w) <> "> cannot contain other elements"
  pure $ Element 
    { elementWidget = w
    , elementFields = values
    , elementChildren = children 
    }


resolveValue :: ViewModel -> String -> AttrSpec -> Maybe (Pos, AttrValue) -> Either ResolveError Value
resolveValue _ _ spec Nothing = Right (defaultFor spec)
resolveValue vm attr spec (Just (at, v)) = 
  case v of
    Binding f -> bindTo f
    Literal s -> literalFor s
  where
    vmName = viewModelName vm
    vmFields = viewModelFields vm
    bindTo f = 
      case find (\(n, _, _) -> n == f) vmFields of
        Nothing -> Left $ ResolveError at $ T.pack vmName <> " has no field '" <> T.pack f <> "'"
        Just (_, sel, ty) -> Right $ case spec of
          DisplayAttrSpec _  | isA ''Field ty    -> ValueOf (ViewModelField sel)
          EditableAttrSpec _ | isA ''Behavior ty -> ReadOnly (ViewModelField sel)
          _                                      -> ViewModelField sel

    literalFor s = 
      case spec of
        DisplayAttrSpec VText  -> Right (TextConst s)
        DisplayAttrSpec VBool  -> BoolConst <$> parseBool s
        EditableAttrSpec VText -> Right (ReadOnly (TextConst s))
        EditableAttrSpec VBool -> ReadOnly . BoolConst <$> parseBool s
        ActionAttrSpec -> Left $ ResolveError at $ "attribute '" <> T.pack attr <> "' must be a binding to a Command"

    parseBool "true"  = Right True
    parseBool "false" = Right False
    parseBool s = Left $ ResolveError at $ "expects true or false, or a binding like {field}; got " <> T.pack (show s)

    isA c (AppT (ConT c') _) = c == c'
    isA _ _ = False

defaultFor :: AttrSpec -> Value
defaultFor spec = case spec of
  DisplayAttrSpec VText  -> TextConst ""
  DisplayAttrSpec VBool  -> BoolConst True
  EditableAttrSpec VText -> ReadOnly (TextConst "")
  EditableAttrSpec VBool -> ReadOnly (BoolConst True)
  ActionAttrSpec         -> NoCommand

generate :: Element -> Q Exp
generate root = do
  vm <- newName "vm"
  body <- element vm root
  pure (LamE [VarP vm] body)

element :: Name -> Element -> Q Exp
element vm e = do
  let w = elementWidget e
  fields <- forM (elementFields e) $ \(field, v) -> (,) field <$> value vm v
  let build extra = VarE (widgetBuild w) `AppE` RecConE (widgetCtor w) (fields ++ extra)
  case widgetChildren w of
    Nothing -> pure $ build []
    Just field -> do
      childrenExps <- mapM (element vm) $ elementChildren e
      cs <- newName "children"
      [| sequence $(pure (ListE childrenExps)) >>= $(pure (LamE [VarP cs] (build [(field, VarE cs)]))) |]

value :: Name -> Value -> Q Exp
value vm v = case v of
  ElementId i      -> [| T.pack <$> i |]
  ViewModelField s -> pure (VarE s `AppE` VarE vm)
  ValueOf v'       -> [| fieldValue $(value vm v') |]
  ReadOnly v'      -> [| readOnlyField $(value vm v') |]
  TextConst s      -> [| pure (T.pack s) |]
  BoolConst b      -> [| pure b |]
  NoCommand        -> [| noCommand |]
