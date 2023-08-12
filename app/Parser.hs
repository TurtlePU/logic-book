{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative (liftA2, (<|>))
import Control.Applicative.Combinators (between, many)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, pack)
import Data.Void (Void)
import Logic.ModalProp (ModalProp (..))
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, parse)
import Text.Megaparsec.Byte.Lexer (lexeme, symbol)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space)

type ParseError = ParseErrorBundle Text Void

type Parser = Parsec Void Text

parseProp :: String -> Text -> Either ParseError (ModalProp Text)
parseProp = parse (expr <* eof)
  where
    expr :: Parser (ModalProp Text)
    expr =
      makeExprParser
        (parens "(" expr ")" <|> fmap Var var)
        [ [ prefix "!" Not,
            prefix "[]" Box,
            prefix "<>" Diamond
          ],
          [infixL "/\\" (:/\:)],
          [infixL "\\/" (:\/:)],
          [infixR "->" (:->:)]
        ]

    var :: Parser Text
    var = lexeme space . fmap pack $ liftA2 (:) letterChar (many alphaNumChar)

    parens :: Text -> Parser a -> Text -> Parser a
    parens open parser close = between (sym open) (sym close) parser

    prefix :: Text -> (a -> a) -> Operator Parser a
    prefix name f = Prefix (f <$ sym name)

    infixL, infixR :: Text -> (a -> a -> a) -> Operator Parser a
    infixL name f = InfixL (f <$ sym name)
    infixR name f = InfixR (f <$ sym name)

    sym :: Text -> Parser Text
    sym = symbol space
