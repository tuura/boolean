{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tuura.Boolean.Parser (
    Expr (..),
    parseExpr, parseWrapper) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Prelude hiding (not, or, and)

data Expr a = Var a
          | Not (Expr a)
          | And (Expr a) (Expr a)
          | Or (Expr a) (Expr a)
          | SubExpr (Expr a)
            deriving (Functor, Foldable, Traversable, Show)

instance Applicative Expr where
    pure  = Var
    (<*>) = ap

instance Monad Expr where
    return = pure

    Var a     >>= f = f a
    Not x     >>= f = Not     (x >>= f)
    And x y   >>= f = And     (x >>= f) (y >>= f)
    Or  x y   >>= f = Or      (x >>= f) (y >>= f)
    SubExpr x >>= f = SubExpr (x >>= f)

parseExpr :: String -> Either ParseError (Expr String)
parseExpr = parse expr ""
  where expr = buildExpressionParser operators term <?> "compound expression"
        term = parens expr <|> variable <?> "full expression"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not),
                       Prefix (string "!" >> return Not),
                       Postfix (string "'" >> spaces >> return Not),
                       Postfix (string "-" >> spaces >> return Not),
                       Postfix (string "+" >> spaces >> return (Not . Not))]
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
