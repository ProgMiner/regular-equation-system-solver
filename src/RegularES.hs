module RegularES
    ( solveForVariable
    , solve
    , showRegEq
    , showRegExp
    , humanifyRegExp
    , RegExp (..)
    , RegEq
    , RegularES
    ) where

import Data.List (foldl', foldl1', sort, group)
import Data.Maybe (fromMaybe)
import Debug.Trace


data RegExp
    = REVar String
    | REStr String
    | REConcat RegExp RegExp
    | REUnion RegExp RegExp
    | REIteration RegExp
    deriving (Show, Read, Eq)

type RegEq = (String, RegExp)
type RegularES = [RegEq]

data ExtRETerm
    = EREVar String
    | EREStr String
    | EREIteration ExtRE
    deriving (Show, Eq)

newtype ExtREConcat = EREConcat [ExtRETerm] deriving (Show, Eq)
newtype ExtREUnion = EREUnion [ExtREConcat] deriving (Show, Eq)
type ExtRE = ExtREUnion


toExtendedRE    -- convert RegExp to extended form
    :: RegExp   -- expression
    -> ExtRE    -- extended expression
toExtendedRE = EREUnion . (map $ EREConcat . map toExtendedRETerm . concatList) . unionList where

    unionList :: RegExp -> [RegExp]
    unionList (REConcat a b) = [REConcat a' b' | a' <- unionList a, b' <- unionList b]
    unionList (REUnion  a b) = unionList a ++ unionList b
    unionList exp = [exp]

    concatList :: RegExp -> [RegExp]
    concatList (REConcat a b) = concatList a ++ concatList b
    concatList exp = [exp]

    toExtendedRETerm :: RegExp -> ExtRETerm
    toExtendedRETerm (REVar       var) = EREVar var
    toExtendedRETerm (REStr       str) = EREStr str
    toExtendedRETerm (REIteration exp) = EREIteration $ toExtendedRE exp
    toExtendedRETerm exp               = error $ show exp ++ " is not an extended RE term"

fromExtendedRE  -- convert RegExp from extended form
    :: ExtRE    -- extended expression
    -> RegExp   -- expression
fromExtendedRE = fromEREUnion where

    fromEREUnion :: ExtREUnion -> RegExp
    fromEREUnion (EREUnion e) = foldl1' REUnion $ map fromEREConcat e

    fromEREConcat :: ExtREConcat -> RegExp
    fromEREConcat (EREConcat e) = foldl1' REConcat $ map fromERETerm e

    fromERETerm :: ExtRETerm -> RegExp
    fromERETerm (EREVar var) = REVar var
    fromERETerm (EREStr str) = REStr str
    fromERETerm (EREIteration exp) = REIteration $ fromExtendedRE exp

findTermsBySuffix   -- find terms in RegExp by suffix(es)
    :: RegExp   -- suffix
    -> RegExp   -- expression
    -> [RegExp] -- grouped for suffix expression
findTermsBySuffix suffix exp = [fromExtendedRE $ EREUnion $ [EREConcat e]
                               | (EREConcat s) <- extSuffix
                               , (EREConcat e) <- extExp
                               , isSuffix s e] where
    (EREUnion extSuffix) = toExtendedRE suffix
    (EREUnion extExp) = toExtendedRE exp

    isSuffix :: (Eq a) => [a] -> [a] -> Bool
    isSuffix as bs = isPrefix (reverse as) (reverse bs)

    isPrefix :: (Eq a) => [a] -> [a] -> Bool
    isPrefix  []     _     = True
    isPrefix  _      []    = False
    isPrefix (a:as) (b:bs) = (a == b) && (isPrefix as bs)

listREVars      -- list variables mentioned in RegExp
    :: RegExp   -- regular expression
    -> [String] -- list of variables
listREVars (REVar var) = [var]
listREVars (REStr _  ) = []
listREVars (REConcat a b) = listREVars a ++ listREVars b
listREVars (REUnion  a b) = listREVars a ++ listREVars b
listREVars (REIteration exp) = listREVars exp

sortRESystem    -- sort RegEq system in solving order
    :: String       -- starting state
    -> RegularES    -- source system
    -> RegularES    -- sorted system (from reg eq w/o deps)
sortRESystem start system = sortRESystem' [start] [] where

    sortRESystem' :: [String] -> RegularES -> RegularES
    sortRESystem' []      visited = visited
    sortRESystem' current visited = sortRESystem' next (result ++ visited) where
        (Just eqs) = mapM (flip lookup system) current
        next = map head $ group $ sort $ filter (flip notElem current)
            . filter (flip notElem $ map fst visited) . listREVars =<< eqs
        result = zip current eqs

substituteVariable :: RegEq -> RegExp -> RegExp
substituteVariable (var, src) dst@(REVar v) = if v == var then src else dst
substituteVariable  _         dst@(REStr _) = dst
substituteVariable eq (REConcat a b) = REConcat (substituteVariable eq a) (substituteVariable eq b)
substituteVariable eq (REUnion a b)  = REUnion  (substituteVariable eq a) (substituteVariable eq b)
substituteVariable eq (REIteration a) = REIteration $ substituteVariable eq a

simplifyEquation :: RegEq -> RegEq
simplifyEquation (var, exp) = (,) var $ fromMaybe exp result where
    result = uncurry (REConcat . REIteration) <$> liftMaybePair (splitBySuffix exp)

    liftMaybePair :: (Maybe a, Maybe b) -> Maybe (a, b)
    liftMaybePair (Just a, Just b) = Just (a, b)
    liftMaybePair  _               = Nothing

    splitBySuffix :: RegExp -> (Maybe RegExp, Maybe RegExp)
    splitBySuffix exp@(REVar v) = if v == var then (Just $ REStr "", Nothing) else (Nothing, Just exp)
    splitBySuffix exp@(REStr _) = (Nothing, Just exp)
    splitBySuffix exp@(REUnion a b) = merge (splitBySuffix a) (splitBySuffix b) where

        merge :: (Maybe RegExp, Maybe RegExp) -> (Maybe RegExp, Maybe RegExp)
            -> (Maybe RegExp, Maybe RegExp)
        merge (Just al, Just ar) (Just bl, Just br) = (Just $ REUnion al bl, Just $ REUnion ar br)
        merge (Just al, Just ar) (Just bl, Nothing) = (Just $ REUnion al bl, Just           ar   )
        merge (Just al, Just ar) (Nothing, Just br) = (Just           al   , Just $ REUnion ar br)
        merge (Just al, Just ar) (Nothing, Nothing) = (Just           al   , Just           ar   )
        merge (Just al, Nothing) (Just bl, Just br) = (Just $ REUnion al bl, Just              br)
        merge (Just al, Nothing) (Just bl, Nothing) = (Just $ REUnion al bl, Nothing             )
        merge (Just al, Nothing) (Nothing, Just br) = (Just           al   , Just              br)
        merge (Just al, Nothing) (Nothing, Nothing) = (Just           al   , Nothing             )
        merge (Nothing, Just ar) (Just bl, Just br) = (Just              bl, Just $ REUnion ar br)
        merge (Nothing, Just ar) (Just bl, Nothing) = (Just              bl, Just           ar   )
        merge (Nothing, Just ar) (Nothing, Just br) = (Nothing             , Just $ REUnion ar br)
        merge (Nothing, Just ar) (Nothing, Nothing) = (Nothing             , Just           ar   )
        merge (Nothing, Nothing) (Just bl, Just br) = (Just              bl, Just              br)
        merge (Nothing, Nothing) (Just bl, Nothing) = (Just              bl, Nothing             )
        merge (Nothing, Nothing) (Nothing, Just br) = (Nothing             , Just              br)
        merge (Nothing, Nothing) (Nothing, Nothing) = (Nothing             , Nothing             )

    splitBySuffix exp@(REConcat a b) = (REConcat a <$> bl, REConcat a <$> br) where
        (bl, br) = splitBySuffix b

    splitBySuffix exp@(REIteration _) = (Nothing, Just exp)

solveForVariable :: String -> RegularES -> RegExp
solveForVariable = ((snd . simplifyEquation . head . solveForVariable') .) . sortRESystem where

    solveForVariable' :: RegularES -> RegularES
    solveForVariable' system@[_]             = system
    solveForVariable' (eq@(var, exp):system) = solveForVariable'
        $ map (fmap $ substituteVariable $ simplifyEquation eq) system

solve :: RegularES -> RegularES
solve system = undefined

showRegEq :: RegEq -> String
showRegEq (var, exp) = var ++ " = " ++ showRegExp exp ++ "\n"

showRegExp :: RegExp -> String
showRegExp (REVar var) = var
showRegExp (REStr str) = show str
showRegExp (REUnion a b) = "(" ++ showRegExp a ++ " + " ++ showRegExp b ++ ")"
showRegExp (REConcat a b) = "(" ++ showRegExp a ++ " . " ++ showRegExp b ++ ")"
showRegExp (REIteration a) = "(" ++ showRegExp a ++ ")*"

humanifyRegExp :: RegExp -> String
humanifyRegExp (REVar var) = "(var " ++ var ++ ")"
humanifyRegExp (REStr str) = str
humanifyRegExp (REUnion a b) = humanifyRegExp a ++ "|" ++ humanifyRegExp b
humanifyRegExp (REConcat a b) = wrap a ++ wrap b where

    wrap :: RegExp -> String
    wrap exp@(REUnion _ _) = "(" ++ humanifyRegExp exp ++ ")"
    wrap exp = humanifyRegExp exp

humanifyRegExp (REIteration a) = "(" ++ humanifyRegExp a ++ ")*"
