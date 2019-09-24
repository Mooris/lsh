module Expander (
    expander
  , syntaxRules
) where

import LispVal

import Data.Maybe

syntaxRules :: [LispVal] -> [LispVal] -> LispVal
syntaxRules literals rules = PrimitiveFunc (expander literals rules)

matchElipsis :: LispVal -> [LispVal] -> ([LispVal], [(String, LispVal)])
matchElipsis _ [] = ([], [])
matchElipsis v@(List a) val@(List l : rest) = case matchElipsis' a l of
  Just match -> let (leRest, matches) = matchElipsis v rest
                in (leRest, match ++ matches)
  Nothing -> (val, [])
matchElipsis val r@(expr : xs) = case matchElipsis' [val] [expr] of
  Just match -> let (leRest, matches) = matchElipsis val xs
                in (leRest, match ++ matches)
  Nothing -> (r, [])

matchElipsis' :: [LispVal] -> [LispVal] -> Maybe [(String, LispVal)]
matchElipsis' [] [] = Just []
matchElipsis' (Atom ato : rRest) (expr : sRest) = case matchElipsis' rRest sRest of
  Just match -> Just ((ato, expr) : match)
  Nothing -> Just [(ato, expr)]
matchElipsis' _ _ = Nothing

isMatch :: [LispVal] -> LispVal -> LispVal -> Maybe [(String, LispVal)]
isMatch lit (List (Atom _ : restRule)) (List (Atom _ : restSyntax)) =
  isMatch' lit restRule restSyntax
isMatch _ _ _ = Nothing

isLit :: String -> [LispVal] -> Bool
isLit _ [] = False
isLit itm (Atom x:_) | itm == x = True
isLit itm (_:xs) | otherwise = isLit itm xs


isMatch' :: [LispVal] -> [LispVal] -> [LispVal] -> Maybe [(String, LispVal)]
isMatch' _ [] [] = Just []
isMatch' lit [Atom a] [Atom b] | isLit a lit = if a == b then Just [] else Nothing
isMatch' _ [Atom a] [expr] = Just [(a, expr)]
isMatch' lit (val@(List _) : Elipsis : rRest) sntx =
  let (sRest, eMatches) = matchElipsis val sntx
  in case isMatch' lit rRest sRest of
    Just match -> Just (eMatches ++ match)
    Nothing -> Nothing
isMatch' lit (List l : rRest) (List e : sRest) = case isMatch' lit l e of
  Just match -> case isMatch' lit rRest sRest of
    Just m -> Just (match ++ m)
    Nothing -> Nothing
  Nothing -> Nothing
isMatch' lit (Atom a : Elipsis : rest) syntax | isLit a lit =
  isMatch' lit rest (matcher syntax)
 where
   matcher (Atom x : xs) = if x == a then matcher xs else xs
   matcher _ = []
isMatch' lit (val@(Atom _) : Elipsis : rest) syntax =
  let (sRest, eMatches) = matchElipsis val syntax
  in case isMatch' lit rest sRest of
    Just match -> Just (eMatches ++ match)
    Nothing -> Nothing
isMatch' lit (Atom a : rest) (Atom expr : syntaxRest) | isLit a lit =
  if a == expr then isMatch' lit rest syntaxRest else Nothing
isMatch' lit (Atom a : rest) (expr : syntaxRest) =
  case isMatch' lit rest syntaxRest of
    Just match -> Just ((a, expr) : match)
    Nothing -> Nothing
isMatch' _ _ _ = Nothing

transformIf ::  (LispVal -> LispVal -> Maybe [(String, LispVal)])
                -> [LispVal]
                -> LispVal
                -> LispVal
transformIf predi [List [rule, expr]] syntax = case predi rule syntax of
  Just match -> reWrite match NoExpand expr
  Nothing -> NoValue
transformIf predi (List [rule, expr] : rest) syntax = case predi rule syntax of
  Just match -> reWrite match NoExpand expr
  Nothing -> transformIf predi rest syntax
transformIf _ _ _ = NoValue

data Mode = Expand | NoExpand

reWrite :: [(String, LispVal)] -> Mode -> LispVal -> LispVal
reWrite match NoExpand val@(Atom a) = fromMaybe val $ lookup a match
reWrite match Expand (Atom a) = List $ lookup' a match
reWrite match Expand (List l) = List $ reWriteList (map prepareArgs l)
 where
  prepareArgs str = lookup' (show str) match
  reWriteList lst@(x:_) | length x > 1 = List (map head lst) : reWriteList (map tail lst)
  reWriteList ([] : _) = []
  reWriteList lst = return . List $ map head lst
reWrite match _ (List l) = List $ rw (reWrite match) l
reWrite _ _ other = other

rw :: (Mode -> LispVal -> LispVal) -> [LispVal] -> [LispVal]
rw _ [] = []
rw fn (lst : Elipsis : rest) = case fn Expand lst of
  List l -> l ++ rw fn rest
  other -> other : rw fn rest
rw fn (lst : rest) = fn NoExpand lst : rw fn rest

lookup' :: (Show a, Show b, Eq a) => a -> [(a, b)] -> [b]
lookup' val args = map snd $ filter ((val ==) . fst) args

expander :: [LispVal] -> [LispVal] -> [LispVal] -> LispVal
expander lit rules [s] =
  let res = transformIf (isMatch lit) rules s
  in Syntax res {-trace ("\nExpanding:\t" ++ show s ++ "\n"
          ++ prettyShow rules
          ++ "\nNowBecomes:\t" ++ show res ++ "\n") $ Syntax res
-}
expander _ _ _ = String "Hmm sorry me no speak english"
