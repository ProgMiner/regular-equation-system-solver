module RegularES where

import Data.List (foldl', foldl1', sort, group)
import Debug.Trace


data RegExp
    = REVar String
    | REStr String
    | REConcat RegExp RegExp
    | REUnion RegExp RegExp
    | REIteration RegExp
    deriving (Show, Eq)

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

toExtendedRETerm
    :: RegExp       -- expression
    -> ExtRETerm    -- extended expression term
toExtendedRETerm (REVar       var) = EREVar var
toExtendedRETerm (REStr       str) = EREStr str
toExtendedRETerm (REIteration exp) = EREIteration $ toExtendedRE exp
toExtendedRETerm exp               = error $ show exp ++ " is not an extended RE term"

toExtendedRE
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

fromExtendedRE
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

findTermsBySuffix
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

listREVars :: RegExp -> [String]
listREVars (REVar var) = [var]
listREVars (REStr _  ) = []
listREVars (REConcat a b) = listREVars a ++ listREVars b
listREVars (REUnion  a b) = listREVars a ++ listREVars b
listREVars (REIteration exp) = listREVars exp

-- sortRESystem    -- sort RegEq system in solving order
--     :: String       -- starting state
--     -> RegularES    -- source system
--     -> RegularES    -- sorted system (from reg eq w/o deps)
-- sortRESystem start system = reverse $ snd $ dfsFromVariable ([], []) start where
--
--     dfsFromVariable :: ([String], RegularES) -> String -> ([String], RegularES)
--     dfsFromVariable (visited, ans) var | var `elem` visited = (visited, ans)
--     dfsFromVariable acc            var | otherwise          = dfs acc eq where
--         (Just eq) = ((,) var) <$> lookup var system
--
--     dfs :: ([String], RegularES) -> RegEq -> ([String], RegularES)
--     dfs (visited, ans) (var, exp) = (newVisited, (var, exp):newAns) where
--         (newVisited, newAns) = foldl' dfsFromVariable (var:visited, ans) $ listREVars exp

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

solveForVariable :: String -> RegularES -> RegExp
solveForVariable var system = undefined

solve :: RegularES -> RegularES
solve system = undefined

traceMsgShowId :: (Show a) => String -> a -> a
traceMsgShowId msg x = trace (msg ++ show x) x
