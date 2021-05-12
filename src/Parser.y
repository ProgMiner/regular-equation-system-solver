-- # vim:syntax=yacc

{
module Parser (parse) where

import Token
import RegularES
}

%name parse
%tokentype { Token }
%error { parsingError }
%monad { Either String } { (>>=) } { return }

%token
    '='     { LetToken }
    '.'     { DotToken }
    '+'     { PlusToken }
    '('     { LBraceToken }
    ')'     { RBraceToken }
    EPS     { EpsToken }
    STRING  { StringToken $$ }
    NAME    { NameToken $$ }

%left '+'
%left '.'

%%

program
    : stmts         { reverse $1 }

stmts
    : {- empty -}   { [] }
    | stmts stmt    { $2 : $1 }

stmt
    : NAME '=' expr { ($1, $3) }

expr
    : '(' expr ')'  { $2 }
    | expr '.' expr { REConcat $1 $3 }
    | expr '+' expr { REUnion $1 $3 }
    | EPS           { REStr "" }
    | STRING        { REStr $1 }
    | NAME          { REVar $1 }

{
parsingError ts = Left $ "Parsing error at " ++ show ts
}
