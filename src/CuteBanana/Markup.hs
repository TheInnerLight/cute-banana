{-# LANGUAGE OverloadedStrings #-}

module CuteBanana.Markup
  ( Pos (..)
  , AttrValue (..)
  , Attr (..)
  , Node (..)
  , parseMarkup
  ) where

import Control.Monad (void, when)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Pos = Pos 
  { posLine :: !Int
  , posCol :: !Int 
  }
  deriving (Eq, Show)

data AttrValue 
  = Binding String 
  | Literal String
  deriving (Eq, Show)

data Attr = Attr
  { attrName :: String
  , attrValue :: AttrValue
  , attrPos :: Pos
  , attrValuePos :: Pos
  } deriving (Show)

data Node = Node
  { nodeName :: String
  , nodeAttrs :: [Attr]
  , nodeChildren :: [Node]
  , nodePos :: Pos
  } deriving (Show)

type Parser = Parsec Void Text

parseMarkup :: FilePath -> Text -> Either String Node
parseMarkup path src = 
  case parse document path src of
    Left bundle -> Left (errorBundlePretty bundle)
    Right node  -> Right node

stripComments :: Parser ()
stripComments = L.space space1 empty (L.skipBlockComment "<!--" "-->")

symbol :: Text -> Parser Text
symbol = L.symbol stripComments

pos :: Parser Pos
pos = do
  p <- getSourcePos
  pure $ Pos (unPos (sourceLine p)) (unPos (sourceColumn p))

name :: String -> Parser String
name what = label what $
  (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf ("_-.:" :: String))

identifier :: Parser String
identifier = label "a field name" $
  (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf ("_'" :: String))

failAt :: Int -> String -> Parser a
failAt off msg = parseError (FancyError off (Set.singleton (ErrorFail msg)))

document :: Parser Node
document = stripComments *> element <* (eof <?> "end of file")

element :: Parser Node
element = do
  p <- pos
  void $ char '<'
  tag <- name "an element name"
  stripComments
  attrs <- many attribute
  childNodes <- ([] <$ symbol "/>") <|> (symbol ">" *> taggedElements tag p)
  pure (Node tag attrs childNodes p)

taggedElements :: String -> Pos -> Parser [Node]
taggedElements tag open = do
  childElements <- many (notFollowedBy (string "</") *> element)
  offset <- getOffset
  next <- lookAhead (optional anySingle)
  case next of
    Nothing -> failAt offset $ "<" <> tag <> "> opened at line " <> show (posLine open) <> " is never closed"
    Just c | c /= '<' -> failAt offset
      "text content is not supported; put text in an attribute such as text=\"...\""
    _ -> pure ()
  void $ string "</"
  end <- name "a closing tag name"
  when (end /= tag) $
    failAt offset $ "closing tag </" <> end <> "> does not match <" <> tag <> "> opened at line " <> show (posLine open)
  stripComments
  void $ symbol ">"
  pure childElements

attribute :: Parser Attr
attribute = do
  p <- pos
  n <- name "an attribute name"
  stripComments
  void $ symbol "="
  void $ char '"'
  vp <- pos
  v <- value
  void $ char '"'
  stripComments
  pure $ Attr n v p vp

value :: Parser AttrValue
value = binding <|> literal
  where
    binding = Binding <$> (char '{' *> hspace *> identifier <* hspace <* (char '}' <?> "'}' to close the binding"))
    literal = Literal . T.unpack <$> takeWhileP (Just "an attribute value") (/= '"')
