module Main (main) where

import Prelude hiding (lex)
import System.Environment (getArgs, getProgName)
import Data.Maybe (listToMaybe, fromMaybe)

import RegularES
import Parser
import Lexer


test1 =
    [ ("S1", REUnion
        (REConcat (REStr "c") (REVar "S1"))
        (REConcat (REStr "a") (REVar "S2"))
      )
    , ("S2", REConcat (REStr "b") (REVar "S3"))
    , ("S3", REUnion
        (REConcat (REStr "a") (REVar "S3"))
        (REUnion
            (REConcat (REStr "b") (REVar "S3"))
            (REUnion (REConcat (REStr "c") (REVar "S3")) (REStr ""))
        )
      )
    ]

test2 =
    [ ("S1", REUnion (REConcat (REStr "1") (REVar "S2")) (REUnion (REConcat (REStr "0") (REVar "S3")) (REStr "")))
    , ("S2", REUnion (REConcat (REStr "1") (REVar "S1")) (REConcat (REStr "0") (REVar "S4")))
    , ("S3", REUnion (REConcat (REStr "0") (REVar "S1")) (REConcat (REStr "1") (REVar "S4")))
    , ("S4", REUnion (REConcat (REStr "0") (REVar "S2")) (REConcat (REStr "1") (REVar "S3")))
    ]

main :: IO ()
main = do
    args <- getArgs

    case args of
        (filename:args1) -> runFile filename (listToMaybe args1)
        _ -> ("Usage: " ++) <$> (++ " <filename> [starting state]") <$> getProgName >>= putStrLn

runFile
    :: String       -- filename
    -> Maybe String -- starting state
    -> IO ()
runFile filename start = do
    file <- readFile filename

    case lex file >>= parse of
        (Right res) -> case res of
            [] -> putStrLn "No equations presented"
            ((firstState, _):_) -> do
                putStrLn "Input:"
                mapM_ (putStr . showRegEq) res

                let startState = fromMaybe firstState start
                let solution = solveForVariable startState res
                putStrLn $ "Solution for state " ++ startState ++ ": " ++ humanifyRegExp solution
        (Left err) -> putStrLn err
