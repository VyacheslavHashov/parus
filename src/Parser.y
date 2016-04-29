-- Synt.y -*- mode: haskell -*-
{
module Parser where
import Control.Monad.Except
import Lexer
import AST
}

-- Lexer structure
%tokentype { Token }

-- Entry point
%name program

-- Parser monad
%monad {Except String} { (>>=) } { pure }
%error { parseError }

%token
   int      { TInt $$ }
   ident    { TIdent $$ }
   '+'      { TPlus }
   '-'      { TMinus }
   '*'      { TProduct }
   '/'      { TDiv }
   '!'      { TNot }
   '&&'     { TAnd }
   '||'     { TOr }
   '>'      { TGt }
   '>='     { TGte }
   '<'      { TLt }
   '<='     { TLte }
   '=='     { TEq }
   '!='     { TNeq }
   if       { TIf }
   else     { TElse }
   while    { TWhile }
   return   { TReturn }
   '='      { TAssign }
   ','      { TComma }
   ';'      { TSemicolon }
   '('      { TLeftParen }
   ')'      { TRightParen }
   '{'      { TLeftBracket }
   '}'      { TRightBracket }
%%



Program 
    : program_                  { reverse $1}

program_ 
    : GlInstruction  { [$1] }
    | program_ GlInstruction { $2 : $1 }

GlInstruction 
    : ident ident ';' { GlVarDecl $2 $1 }
    | ident ident '(' ArgDeclList ')' CodeBlock { FunDecl $2 $4 $6 $1 }

ArgDeclList 
    : ArgDecl         { [$1] }
    | ArgDecl ',' ArgDeclList { $1 : $3 }

ArgDecl 
    : ident ident   { ArgDecl $2 $1 }

CodeBlock 
    : codeblock_      { reverse $1 }

codeblock_ 
    : Instruction { [$1] }
    | codeblock_ Instruction { $2 : $1 }

Instruction 
    : ident ident ';'   { VarDecl $2 $1 }
    | ident '=' Expr ';' { Assign $1 $3 }
    | return ';'   { Return Nothing }
    | return Expr ';' { Return $ Just $2 }
    | if '(' Expr ')' CodeBlock else CodeBlock { IfElseBlock $3 $5 $7 }
    | if '(' Expr ')' CodeBlock { IfBlock $3 $5 }
    | while '(' Expr ')' CodeBlock { WhileBlock $3 $5 }

Expr 
    : Expr BinOpType Expr     { BinOp $2 $1 $3 }
    | UnOpType Expr           { UnOp $1 $2 }
    | ident '(' ArgList ')'  { FunApply $1 $3 }
    | int                     { Atom $1 }

BinOpType
    : '+'   { OpPlus }
    | '-'   { OpMinus }
    | '*'   { OpProduct }
    | '/'   { OpDivision }
    | '&&'  { OpAnd }
    | '||'  { OpOr }
    | '>'   { OpGt }
    | '>='  { OpGte }
    | '<'   { OpLt }
    | '<='  { OpLte }
    | '=='  { OpEq }
    | '!='  { OpNeq }

UnOpType
    : '!'   { OpNot }

ArgList 
    : Expr         { [$1] }
    | Expr ',' ArgDeclList { $1 : $3 }


{
parseError :: [Token] -> Except String a
parseError (t:ts) = throwError $ show t
parseError [] = throwError "Unexpected end of input"
}
