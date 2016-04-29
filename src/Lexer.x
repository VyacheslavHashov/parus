-- Lex.x -*- mode: haskell -*s
{
module Lexer where
import Numeric
}

%wrapper "monad"

$whitechar = [ \t\r\n\f\v]
$alpha = [a-zA-Z]
$digit = [0-9]

@number = \-?[$digit]+
@float = \-?[$digit]*\.[$digit]+
@ident = $alpha($alpha|_|$digit)*

tokens :-

<0> $white+             { skip }

<0> @float              { mkLiteral }
<0> @number             { mkLiteral }

<0> "true"              { mkL LTrue }
<0> "false"             { mkL LFalse }
<0> "void"              { mkL LVoid }
<0> "bool"              { mkL LBool }
<0> "int"               { mkL LInt }
<0> "uint"              { mkL LUint }
<0> "float"             { mkL LFloat }
<0> "if"                { mkL LIf }
<0> "else"              { mkL LElse }
<0> "while"             { mkL LWhile }
<0> "return"            { mkL LReturn }
<0> "+"                 { mkL LPlus }
<0> "-"                 { mkL LMinus }
<0> "*"                 { mkL LProduct }
<0> "/"                 { mkL LDiv }
<0> ">="                { mkL LGte }
<0> ">"                 { mkL LGt }
<0> "<="                { mkL LLte }
<0> "<"                 { mkL LLt }
<0> "=="                { mkL LEq }
<0> "!="                { mkL LNeq }
<0> "&&"                { mkL LAnd }
<0> "||"                { mkL LOr }
<0> "!"                 { mkL LNot }
<0> "="                 { mkL LAssign }
<0> ","                 { mkL LComma }
<0> ";"                 { mkL LSemicolon }
<0> "("                 { mkL LLeftParen }
<0> ")"                 { mkL LRightParen }
<0> "{"                 { mkL LLeftBracket }
<0> "}"                 { mkL LRightBracket }

<0> @ident              { mkIdent }

{
data Lexeme = L AlexPosn LexemeClass String
    deriving Show

data LexemeClass = 
    LIdent String
    | LLiteral String
    | LTrue | LFalse
    | LVoid | LBool | LInt | LUint | LFloat
    | LPlus | LMinus | LProduct | LDiv 
    | LGt | LGte | LLt | LLte | LEq | LNeq | LAnd | LOr
    | LNot 
    | LIf | LElse | LWhile | LReturn | LAssign
    | LComma | LSemicolon 
    | LLeftParen | LRightParen 
    | LLeftBracket | LRightBracket
    | LEOF
    deriving (Show, Eq)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = pure $ L p c (take len str)

mkLiteral :: AlexInput -> Int -> Alex Lexeme
mkLiteral (p, _, _, str) len = pure . L p (LLiteral $ take len str) $ take len str

mkIdent :: AlexInput -> Int -> Alex Lexeme
mkIdent (p, _, _, str) len = pure . L p (LIdent $ take len str) $ take len str

alexEOF :: Alex Lexeme
alexEOF = pure $ L undefined LEOF ""

scanner :: String -> Either String [Lexeme]
scanner str = let loop = do lex@(L _ tl _) <- alexMonadScan 
                            if tl == LEOF 
                                then pure []
                                else (lex:) <$> loop
              in runAlex str loop
}
