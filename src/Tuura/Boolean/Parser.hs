{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tuura.Boolean.Parser (
    Expr (..),
    parseExpr, parseWrapper,partialEval) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Prelude hiding (not, or, and)

data Expr a = Val Bool
          | Var a
          | Not (Expr a)
          | And (Expr a) (Expr a)
          | Or (Expr a) (Expr a)
          | SubExpr (Expr a)
            deriving (Functor, Foldable, Traversable, Show)

instance Applicative Expr where
  pure = Var
  Val False <*> _ = Val False 

instance Monad Expr where
  return = Var
  Val False >>= _ = Val False

  Val False >> _ = Val False
  Val True >> e = e

partialEval :: (a -> Maybe Bool) -> Expr a -> Expr a
partialEval f expr = expr >>= substitute
  where
    substitute x = case f x of
        Just bool -> Val bool
        Nothing   -> Var x

parseExpr :: String -> Either ParseError (Expr String)
parseExpr = parse expr ""
  where expr = buildExpressionParser operators term <?> "compound expression"
        term = parens expr <|> variable <?> "full expr ession"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not),
                       Prefix (string "!" >> return Not),
                       Postfix (string "'" >> return Not)]
                    , [binary "AND" And, binary "*" And, binary "&" And],
                      [binary "OR" Or, binary "|" Or, binary "+" Or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var <$> (varParser <* spaces) <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces)
                           <?> "parens"
        varParser = liftM2 (++) (many1 letter) (many alphaNum)

parseWrapper :: String -> Expr String
parseWrapper expr =
  case parseExpr expr of
    Right x -> x
    Left _ -> error $ "Error parsing expression " ++ expr
