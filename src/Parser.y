-- Synt.y -*- mode: haskell -*-
{
module Parser where
import Control.Monad.Except
import Data.Bifunctor
import qualified Data.Map as Map
import Lexer
import AST
}

-- Lexer structure
%tokentype { Lexeme }

-- Entry point
%name program

-- Parser monad
%monad { Except String } { (>>=) } { pure }
%error { parseError }

%token
   literal  { L _ (LLiteral $$) _ }
   ident    { L _ (LIdent $$) _ }
   true     { L _ LTrue _ }
   false    { L _ LFalse _ }
   void     { L _ LVoid _ }
   bool     { L _ LBool _ }
   int      { L _ LInt _ }
   uint     { L _ LUint _ }
   float    { L _ LFloat _ }
   '+'      { L _ LPlus _ }
   '-'      { L _ LMinus _ }
   '*'      { L _ LProduct _ }
   '/'      { L _ LDiv _ }
   '>'      { L _ LGt _ }
   '>='     { L _ LGte _ }
   '<'      { L _ LLt _ }
   '<='     { L _ LLte _ }
   '=='     { L _ LEq _ }
   '!='     { L _ LNeq _ }
   '&&'     { L _ LAnd _ }
   '||'     { L _ LOr _ }
   '!'      { L _ LNot _ }
   if       { L _ LIf _ }
   else     { L _ LElse _ }
   while    { L _ LWhile _ }
   return   { L _ LReturn _ }
   '='      { L _ LAssign _ }
   ','      { L _ LComma _ }
   ';'      { L _ LSemicolon _ }
   '('      { L _ LLeftParen _ }
   ')'      { L _ LRightParen _ }
   '{'      { L _ LLeftBracket _ }
   '}'      { L _ LRightBracket _ }

%left '||'
%left '&&'
%left '==' '!='
%left '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%right '!'
%right NEG

%%

Program 
    : program_                  { AST {
                                   globalVars = Map.fromList $ fst $1
                                 , functions = Map.fromList $ snd $1 } }

program_ :: { ([(VarName, Type)], 
               [(FunName, Function)]) }
program_
    : {- empty -}               { ([], []) }
    | program_ Type ident ';'   { first ((VarName $3, $2):) $1 }
    | program_ Type ident 
     '(' ArgDeclList ')' 
     functionBlock             { let funDecl = Function {
                                    fLocalVars = Map.fromList (fst $7 ++ $5)
                                  , fReturnType = $2
                                  , fArgNames = map fst $5
                                  , fCodeBlock = snd $7 }
                                  in second ((FunName $3, funDecl):) $1 }

ArgDeclList :: { [(VarName, Type)] }
    : {-empty -}                { [] }
    | ArgDecl ArgDeclList1      { $1 : $2 }

ArgDeclList1
    : {- empty -}               { [] }
    | ',' ArgDecl ArgDeclList1  { $2 : $3 }

ArgDecl 
    : Type ident                { (VarName $2, $1) }

functionBlock :: { ([(VarName, Type)], CodeBlock) }
functionBlock
    : '{' functionBlock_ '}'    { second reverse $2 }

functionBlock_
    : {- empty -}               { ([], []) }
    | functionBlock_ Type 
      ident ';'                 { first ((VarName $3, $2):) $1 }
    | functionBlock_ Instruction { second ($2:) $1 }

CodeBlock 
    : '{' codeblock_ '}'        { reverse $2 }

codeblock_ 
    : {- empty -}               { [] }
    | codeblock_ Instruction    { $2 : $1 }

Instruction 
    : ident '=' Expr ';'        { Assign (VarName $1) $3 }
    | return ';'                { Return $ Value VoidValue }
    | return Expr ';'           { Return $2 }
    | if '(' Expr ')' CodeBlock 
      else CodeBlock            { IfElseBlock $3 $5 $7 }
    | if '(' Expr ')' CodeBlock { IfElseBlock $3 $5 []}
    | while '(' Expr ')' 
      CodeBlock                 { WhileBlock $3 $5 }
    | Expr ';'                  { Expr $1 }

Expr 
    : Expr '+' Expr             { BinOp OpPlus $1 $3 }
    | Expr '-' Expr             { BinOp OpMinus $1 $3 }
    | Expr '*' Expr             { BinOp OpProduct $1 $3 }
    | Expr '/' Expr             { BinOp OpDivision $1 $3 }
    | Expr '>' Expr             { BinOp OpGt $1 $3 }
    | Expr '>=' Expr            { BinOp OpGte $1 $3 }
    | Expr '<' Expr             { BinOp OpLt $1 $3 }
    | Expr '<=' Expr            { BinOp OpLte $1 $3 }
    | Expr '==' Expr            { BinOp OpEq $1 $3 }
    | Expr '!=' Expr            { BinOp OpNeq $1 $3 }
    | Expr '&&' Expr            { BinOp OpAnd $1 $3 }
    | Expr '||' Expr            { BinOp OpOr $1 $3 }
    | '(' Expr ')'              { $2 }

    | '-' Expr  %prec NEG       { UnOp OpNegate $2 }
    | '!' Expr                  { UnOp OpNot $2 }

    | ident '(' ArgList ')'     { FunApply (FunName $1) $3 }
    | literal                   { Value $ RawValue $1 }
    | boolLiteral               { Value $1 }
    | ident                     { Ident (VarName $1) }


ArgList 
    : {- empty -}               { [] }
    | Expr ArgList1             { $1 : $2 }

ArgList1
    : {- empty -}               { [] }
    | ',' Expr ArgList1         { $2 : $3 }

boolLiteral
    : true                      { BoolValue True }
    | false                     { BoolValue False }

Type
    : void                      { TVoid }
    | bool                      { TBool }
    | int                       { TInt }
    | uint                      { TUint }
    | float                     { TFloat }

{
parseError :: [Lexeme] -> Except String a
parseError (t:ts) = throwError $ show t
parseError [] = throwError "Unexpected end of input"
}
