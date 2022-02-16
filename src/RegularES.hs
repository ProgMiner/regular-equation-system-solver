module RegularES
    ( solveForVariable
    , showRegEq
    , showRegExp
    , humanifyRegExp
    , RegExp (..)
    , RegEq
    , RegularES
    ) where

import Data.List (sort, group)
import Data.Maybe (fromMaybe)


data RegExp
    = REVar String
    | REStr String
    | REConcat RegExp RegExp
    | REUnion RegExp RegExp
    | REIteration RegExp
    deriving (Show, Read, Eq)

type RegEq = (String, RegExp)
type RegularES = [RegEq]


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

        canBeNext :: String -> Bool
        canBeNext v = notInCurrent && notInVisited where
            notInCurrent = notElem v current
            notInVisited = notElem v $ map fst visited

        (Just eqs) = mapM (flip lookup system) current
        next = uniq $ sort $ filter canBeNext . listREVars =<< eqs
        result = zip current eqs
        uniq = map head . group

substituteVariable :: RegEq -> RegExp -> RegExp
substituteVariable (var, src) dst@(REVar v) = if v == var then src else dst
substituteVariable  _         dst@(REStr _) = dst
substituteVariable eq (REConcat a b) = REConcat (substituteVariable eq a) (substituteVariable eq b)
substituteVariable eq (REUnion a b)  = REUnion  (substituteVariable eq a) (substituteVariable eq b)
substituteVariable eq (REIteration a) = REIteration $ substituteVariable eq a

simplifyEquation -- try to apply Arden's lemma
    :: RegEq     -- source regular equation
    -> RegEq     -- result
simplifyEquation (var, exp) = (,) var $ fromMaybe exp result where
    result = uncurry (REConcat . REIteration) <$> liftMaybePair (splitBySuffix exp)

    liftMaybePair :: (Maybe a, Maybe b) -> Maybe (a, b)
    liftMaybePair (Just a, Just b) = Just (a, b)
    liftMaybePair  _               = Nothing

    splitBySuffix -- split regular expression on part with variable as suffix and without
        :: RegExp -- source regular expression
        ->  ( Maybe RegExp  -- part with suffix
            , Maybe RegExp  -- part without suffix
            )
    splitBySuffix exp@(REVar v) | v == var  = (Just $ REStr "", Nothing)
                                | otherwise = (Nothing, Just exp)

    splitBySuffix exp@(REStr _) = (Nothing, Just exp)
    splitBySuffix (REUnion a b) = merge (splitBySuffix a) (splitBySuffix b) where

        merge
            :: (Maybe RegExp, Maybe RegExp)
            -> (Maybe RegExp, Maybe RegExp)
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

    splitBySuffix (REConcat a b) = (REConcat a <$> bl, REConcat a <$> br) where
        (bl, br) = splitBySuffix b

    splitBySuffix exp@(REIteration _) = (Nothing, Just exp)

solveForVariable
    :: String     -- first state
    -> RegularES  -- equation system
    -> RegExp     -- solution
solveForVariable = (solveSorted .) . sortRESystem where

    solveSorted
        :: RegularES  -- sorted system of equations
        -> RegExp     -- solution of the last equation
    solveSorted [eq]                = snd $ simplifyEquation eq
    solveSorted (eq@(var, exp):eqs) = solveSorted $ map (fmap $ substituteVariable eq') eqs where
        eq' = simplifyEquation eq

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

humanifyRegExp (REIteration a) = wrap (humanifyRegExp a) where

    wrap :: String -> String
    wrap [c] = c:"*"
    wrap s | inBraces s = s ++ "*" where
        inBraces ('(':t) | last t == ')' && bracesValid (reverse $ tail $ reverse t) = True where
            bracesValid :: String -> Bool
            bracesValid = bracesValid' 0

            bracesValid' :: Int -> String -> Bool
            bracesValid' c [] = c == 0
            bracesValid' c (h:t) | c < 0     = False
                                 | h == '('  = bracesValid' (c + 1) t
                                 | h == ')'  = bracesValid' (c - 1) t
                                 | otherwise = bracesValid'  c      t
        inBraces _ = False
    wrap s = "(" ++ s ++ ")*"
