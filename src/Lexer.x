-- Lex.x -*- mode: haskell -*s
{
module Lexer where
import Numeric
}

%wrapper "monad"

$whitechar = [ \t\r\n\f\v]
$alpha = [a-zA-Z]
$digit = [0-9]

@number = [$digit]+
@ident = $alpha($alpha|_|$digit)*

tokens :-

<0> $white+             { skip }
<0> @number             { mkInt }
<0> @ident              { mkIdent }
<0> "+"                 { mkL TPlus }
<0> "-"                 { mkL TMinus }
<0> "*"                 { mkL TProduct }
<0> "/"                 { mkL TDiv }
<0> "!"                 { mkL TNot }
<0> "&&"                { mkL TAnd }
<0> "||"                { mkL TOr }
<0> ">"                 { mkL TGt }
<0> "<"                 { mkL TLt }
<0> ">="                { mkL TGte }
<0> "<="                { mkL TLte }
<0> "=="                { mkL TEq }
<0> "!="                { mkL TNeq }
<0> "if"                { mkL TIf }
<0> "else"              { mkL TElse }
<0> "while"             { mkL TWhile }
<0> "return"            { mkL TReturn }
<0> "="                 { mkL TAssign }
<0> ","                 { mkL TComma }
<0> ";"                 { mkL TSemicolon }
<0> "("                 { mkL TLeftParen }
<0> ")"                 { mkL TRightParen }
<0> "{"                 { mkL TLeftBracket }
<0> "}"                 { mkL TRightBracket }

{
data Token = T AlexPosn TokenClass String
    deriving Show

data TokenClass = 
    TIdent String
    | TInt Int  | TDouble Double 
    | TPlus | TMinus | TProduct | TDiv 
    | TNot 
    | TAnd | TOr | TGt | TGte | TLt | TLte | TEq | TNeq
    | TIf | TElse | TWhile | TReturn | TAssign
    | TComma | TSemicolon | TLeftParen | TRightParen 
    | TLeftBracket | TRightBracket
    | TEOF
    deriving Eq

instance Show TokenClass where
    show x = case x of
        TIdent s -> s
        TInt n -> show n
        TDouble n -> show n
        TPlus -> "+"
        TMinus -> "-"
        TProduct -> "*"
        TDiv -> "/"
        TNot -> "!"
        TAnd -> "&&"
        TOr -> "||"
        TGt -> ">"
        TLt -> "<"
        TGte -> ">="
        TLte -> "<="
        TEq -> "=="
        TNeq -> "!="
        TIf -> "if"
        TElse -> "else"
        TWhile -> "while"
        TReturn -> "return"
        TAssign -> "="
        TComma -> ","
        TSemicolon -> ";"
        TLeftParen -> "("
        TRightParen -> ")"
        TLeftBracket -> "{"
        TRightBracket -> "}"
        TEOF -> "(EOF)"

mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = pure $ T p c (take len str)

mkInt :: AlexInput -> Int -> Alex Token
mkInt (p, _, _, str) len = pure . T p (TInt . read $ take len str) $ take len str

mkIdent :: AlexInput -> Int -> Alex Token
mkIdent (p, _, _, str) len = pure . T p (TIdent $ take len str) $ take len str

alexEOF :: Alex Token
alexEOF = pure $ T undefined TEOF ""

scanner :: String -> Either String [Token]
scanner str = let loop = do tok@(T _ tl _) <- alexMonadScan 
                            if tl == TEOF 
                                then pure []
                                else (tok:) <$> loop
              in runAlex str loop
}
