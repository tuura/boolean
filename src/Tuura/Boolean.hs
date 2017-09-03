{-# LANGUAGE FlexibleInstances #-}

module Tuura.Boolean (
  module Text.PrettyPrint.HughesPJClass,
  module Tuura.Boolean.Parser,
  CNF (..), DNF (..), Literal (..),
  convertToCNF,
  simplifyCNF, simplifyDNF, convertCNFtoDNF,
  getVars, eval, eval') where

import Data.List
import Data.List.Extra
import Data.Foldable
import Data.Maybe
import Data.Ord
import Text.PrettyPrint.HughesPJClass

import Tuura.Boolean.Parser

newtype CNF a = CNF { fromCNF :: [[Literal a]] } deriving Show

instance Pretty (CNF String) where
  pPrint = equationDoc plus star parens . fromCNF

newtype DNF a = DNF { fromDNF :: [[Literal a]] } deriving Show

instance Pretty (DNF String) where
  pPrint = equationDoc star plus id . fromDNF

equationDoc :: Doc -> Doc -> (Doc -> Doc) -> [[Literal String]] -> Doc
equationDoc delim1 delim2 f1 eq = (hcat . intersperse delim2) ors
  where ors = map (f1 . hcat . intersperse delim1) ands
        ands = (map . map) pPrint eq

star :: Doc
star = text "*"

plus :: Doc
plus = text " + "

data Literal a = Literal { variable :: a, polarity :: Bool } deriving (Eq, Ord, Show)

instance Pretty (Literal String) where
  pPrint (Literal var True) = text var
  pPrint (Literal var False) = char '!' <> text var

convertToCNF :: Eq a => Expr a -> CNF a
convertToCNF expr = cnf
  where
    vars = getVars expr
    values = mapM (const [False, True]) vars
    fs = filter (not . eval expr . getValue vars) values
    cnf = CNF $ map (\v -> map (\f -> Literal (vars !! f) (not $ v !! f)) [0..(length vars - 1)]) fs
    getValue vs vals v = fromJust $ lookup v $ zip vs vals

getVars :: Eq a => Expr a -> [a]
getVars = nub . toList

eval :: Expr a -> (a -> Bool) -> Bool
eval (Val a) _     = a
eval (Var a) f     = f a
eval (Not a) f     = not (eval a f)
eval (And a b) f   = eval a f && eval b f
eval (Or a b) f    = eval a f || eval b f
eval (SubExpr a) f = eval a f

-- Partially appliable version of eval
eval' :: Expr a -> (a -> Maybe Bool) -> Expr a
eval' (Val a) _     = Val a
eval' (Var a) f     = case f a of
                        Just x  -> Val x
                        Nothing -> Var a
eval' (Not a) f     = case eval' a f of
                        Val x -> Val $ not x
                        x -> Not x
eval' (And a b) f   = case (eval' a f, eval' b f) of
                        (Val x, Val y) -> Val $ x && y
                        (Val x, y    ) -> if' x y (Val False)
                        (x    , Val y) -> if' y x (Val False)
                        (x    , y    ) -> And x y
eval' (Or a b) f    = case (eval' a f, eval' b f) of
                        (Val x, Val y) -> Val $ x || y
                        (Val x, y    ) -> if' x (Val True) y
                        (x    , Val y) -> if' y (Val True) x
                        (x    , y    ) -> Or x y
eval' (SubExpr a) f = eval' a f

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

invert :: Literal a -> Literal a
invert l = Literal (variable l) (not $ polarity l)

-- Take a DNF and remove any redundancies and supersets for a more compact form.
simplifyDNF :: Ord a => DNF a -> DNF a
simplifyDNF x = DNF (removeRedundancies $ removeSupersets $ fromDNF x)

-- Take a CNF and remove any sets that also exist with one negated variable
-- then remove any supersets that still exist.
simplifyCNF :: Eq a => CNF a -> CNF a
simplifyCNF c = CNF (removeSupersets $ fromCNF $ removeCancels vars c)
  where
    vars = nub $ concatMap (map variable) (fromCNF c)

-- For each variable in the expression, get all subexpressions which contain it
-- then find any of these with the current variable negated, remove these from
-- the expression and add these subexpressions without the current variable.
removeCancels :: Eq a => [a] -> CNF a -> CNF a
removeCancels [] whole = whole
removeCancels (v:vs) whole = removeCancels vs newWhole
  where
    var = Literal v True
    nVar = Literal v False
    relevant = filter (var `elem`) (fromCNF whole)
    nRelevant = map (replace [var] [nVar]) relevant
    existInWhole = filter (`elem` fromCNF whole) nRelevant
    toBeRemoved = map (replace [nVar] [var]) existInWhole
    replacements = map (delete var) toBeRemoved
    newWhole = CNF (fromCNF whole ++ replacements)

-- Apply cartesian product to a CNF function to get DNF,
-- needed to produce STGs and FSMs.
convertCNFtoDNF :: Ord a => CNF a -> DNF a
convertCNFtoDNF l = DNF $ map (sort . nub) (sequence (fromCNF l))

-- Sort list of lists from largest length to shortest, then remove any lists
-- that have shorter subsequences within the rest of the list.
removeSupersets :: Eq a => [[Literal a]] -> [[Literal a]]
removeSupersets s = [ x | (x:xs) <- tails sortByLength, not (check x xs) ]
  where
    check current = any (`isSubsequenceOf` current)
    sortByLength  = sortBy (comparing $ negate . length) s

-- Redundancies are subexpressions where one of the variables exists in another
-- subexpression, but negated. Both of these can be removed from the expression.
removeRedundancies :: Eq a => [[Literal a]] -> [[Literal a]]
removeRedundancies = filter (\ts -> all (\t -> invert t `notElem` ts) ts)
