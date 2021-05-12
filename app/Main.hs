module Main where

import RegularES


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
main = undefined
