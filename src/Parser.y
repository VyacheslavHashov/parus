-- Synt.y -*- mode: haskell -*-
{
module Parser where
import Control.Monad.Except
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
    : program_                  { reverse $1 }

program_ 
    : GlInstruction             { [$1] }
    | program_ GlInstruction    { $2 : $1 }

GlInstruction 
    : Type ident ';'            { RawGlVarDecl $2 $1 }
    | Type ident '(' ArgDeclList 
      ')' CodeBlock             { RawFunDecl $2 $4 $6 $1 }

ArgDeclList 
    : {-empty -}                { [] }
    | ArgDecl ArgDeclList1      { $1 : $2 }

ArgDeclList1
    : {- empty -}               { [] }
    | ',' ArgDecl ArgDeclList1  { $2 : $3 }

ArgDecl 
    : Type ident                { RawArgDecl $2 $1 }

CodeBlock 
    : '{' codeblock_ '}'        { reverse $2 }

codeblock_ 
    : Instruction               { [$1] }
    | codeblock_ Instruction    { $2 : $1 }

Instruction 
    : Type ident ';'            { RawVarDecl $2 $1 }
    | ident '=' Expr ';'        { RawAssign $1 $3 }
    | return ';'                { RawReturn Nothing }
    | return Expr ';'           { RawReturn $ Just $2 }
    | if '(' Expr ')' CodeBlock 
      else CodeBlock            { RawIfElseBlock $3 $5 $7 }
    | if '(' Expr ')' CodeBlock { RawIfBlock $3 $5 }
    | while '(' Expr ')' 
      CodeBlock                 { RawWhileBlock $3 $5 }
    | Expr ';'                  { RawExpr $1 }

Expr 
    : Expr '+' Expr             { RawBinOpAr OpPlus $1 $3 }
    | Expr '-' Expr             { RawBinOpAr OpMinus $1 $3 }
    | Expr '*' Expr             { RawBinOpAr OpProduct $1 $3 }
    | Expr '/' Expr             { RawBinOpAr OpDivision $1 $3 }

    | Expr '>' Expr             { RawBinOpLog OpGt $1 $3 }
    | Expr '>=' Expr            { RawBinOpLog OpGte $1 $3 }
    | Expr '<' Expr             { RawBinOpLog OpLt $1 $3 }
    | Expr '<=' Expr            { RawBinOpLog OpLte $1 $3 }
    | Expr '==' Expr            { RawBinOpLog OpEq $1 $3 }
    | Expr '!=' Expr            { RawBinOpLog OpNeq $1 $3 }
    | Expr '&&' Expr            { RawBinOpLog OpAnd $1 $3 }
    | Expr '||' Expr            { RawBinOpLog OpOr $1 $3 }
    | '(' Expr ')'              { $2 }

    | '-' Expr  %prec NEG       { RawUnOpAr OpNegate $2 }
    | '!' Expr                  { RawUnOpLog OpNot $2 }

    | ident '(' ArgList ')'     { RawFunApply $1 $3 }
    | literal                   { RawLiteral $1 }
    | boolLiteral               { $1 }
    | ident                     { RawIdent $1 }


ArgList 
    : {- empty -}               { [] }
    | Expr ArgList1             { $1 : $2 }

ArgList1
    : {- empty -}               { [] }
    | ',' Expr ArgList1         { $2 : $3 }

boolLiteral
    : true                      { RawBoolLiteral True }
    | false                     { RawBoolLiteral False }

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
