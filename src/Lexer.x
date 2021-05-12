-- # vim:syntax=lex

{
module Lexer (lex) where

import Prelude hiding (lex)
import Data.Char (isHexDigit, chr, digitToInt)

import Token
}

%wrapper "monad"

$eps = [\%Εεϵ]

tokens :-

$white+         ;
"#".*           ;
"="             { constToken LetToken }
"."             { constToken DotToken }
"+"             { constToken PlusToken }
$eps            { constToken EpsToken }
"("             { constToken LBraceToken }
")"             { constToken RBraceToken }
[0-9a-zA-Z_']+  { strToken $ NameToken }

\"(\\.|[^\\\"])*\"   { stringToken $ StringToken }

{
data TokenWrapper = TW Token | WEOF

alexEOF = return WEOF

constToken :: Token -> AlexAction TokenWrapper
constToken t _ _ = return $ TW t

strToken :: (String -> Token) -> AlexAction TokenWrapper
strToken f (_, _, _, str) len = return $ TW $ f $ take len str

posStrToken :: (AlexPosn -> String -> Token) -> AlexAction TokenWrapper
posStrToken f (pos, _, _, str) len = return $ TW $ f pos $ take len str

stringToken :: (String -> Token) -> AlexAction TokenWrapper
stringToken f ((AlexPn _ line column), _, _, str) len =
    case parseString $ init $ tail $ take len str of
        Right s     -> return $ TW $ f s
        Left (i, c) -> alexError $ "undefined escape sequence \\"
            ++ c:" at " ++ show line ++ ':':show (column + i)
  where

    parseString :: String -> Either (Int, Char) String
    parseString = parseString' 1 where

        parseString' :: Int -> String -> Either (Int, Char) String
        parseString' i ('\\':'x':h:l:cs) | all isHexDigit [h, l]
            = ((chr $ readHex [h, l]) :) <$> parseString' (i + 1) cs

        parseString' i ('\\':c:cs) = do
            cs <- parseString' (i + 1) cs

            case escapeChar c of
                Just ec -> Right (ec:cs)
                Nothing -> Left  (i, c)

        parseString' i (c:cs) = (c :) <$> parseString' (i + 1) cs
        parseString' _  []    = Right []

    readHex :: String -> Int
    readHex = snd . foldr f (1, 0) . map digitToInt where
        f :: Int -> (Int, Int) -> (Int, Int)
        f n (p, s) = (p * 16, s + p * n)

    escapeChar :: Char -> Maybe Char
    escapeChar '\\' = Just '\\'
    escapeChar 'n'  = Just '\n'
    escapeChar 't'  = Just '\t'
    escapeChar '"'  = Just '"'
    escapeChar  _   = Nothing


lex :: String -> Either String [Token]
lex str = reverse <$> runAlex str (lex' []) where
    lex' ts = alexMonadScan >>= lex'' where
        lex'' (TW t) = lex' (t:ts)
        lex'' WEOF   = return ts
}
