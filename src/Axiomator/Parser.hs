module Axiomator.Parser
  ( parseUnsafe
  ) where

import Data.String (IsString(..))

import Control.Monad (msum)
import Text.Parsec hiding (State(..))
import Text.Parsec.Expr
import qualified Text.Parsec.Token    as Tok
import qualified Text.Parsec.Language as Tok

import Axiomator.Types

allowedFunctionNames =
  [ "sin"
  , "cos"
  , "tan"
  , "f"
  , "g"
  ]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = Tok.emptyDef
      { Tok.reservedOpNames = ["+", "!", "^", "/", "*"]
      , Tok.reservedNames   = []
      , Tok.identStart      = letter
      }

reservedOp = Tok.reservedOp lexer
whiteSpace = Tok.whiteSpace lexer

parens = between (char '(' <* whiteSpace) (char ')')

termExpr = (parens expr
             <|> Const . read <$> many1 (oneOf ['0'..'9'])
             <|> Var . replicate 1 <$> oneOf ['a'..'z']
             <|> (char '_' >> return Hole)
           ) <* whiteSpace

table = [ [postfix "!" (Op1 Factorial), series "S", limit "lim", functionExpr, prefix "-" (Op1 Negate) ]
        , [binary "^" (Op2 Exponent) AssocLeft ]
        , [binary "*" (Op2 Product) AssocLeft, binary "/" (Op2 Fraction) AssocLeft, binary "" (Op2 Product) AssocLeft]
        , [binary "+" (Op2 Sum) AssocLeft, binary "-" (\a b -> Op2 Sum a (Op1 Negate b)) AssocLeft ]
        ]

functionExpr :: Monad m => Operator String u m Term
functionExpr = Prefix . try $ do
  name <- msum . map string $ allowedFunctionNames
  return $ Op1 (Function name)

series op = Prefix $
  do
    string op
    char '['
    v <- replicate 1 <$> oneOf ['a'..'z']
    whiteSpace
    char '='
    whiteSpace
    i <- expr
    char ']'

    return $ Op2 (Series v) i

limit op = Prefix $
  do
    string op
    char '['
    v <- replicate 1 <$> oneOf ['a'..'z']
    whiteSpace
    string "->"
    whiteSpace
    i <- expr
    char ']'

    return $ Op2 (Limit v) i

prefix name fun = Prefix (do { reservedOp name; return fun })
postfix name fun = Postfix (do { reservedOp name; return fun })
binary name fun assoc = Infix (do { reservedOp name; return fun}) assoc

expr = buildExpressionParser table (whiteSpace *> termExpr)

parseUnsafe input =
  case parse expr input input of
    Right x -> x
    Left x -> error $ "error parsing: " <> input

instance IsString Term where
  fromString cs = parseUnsafe cs
