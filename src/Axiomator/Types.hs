module Axiomator.Types
  ( Term(..)
  , Term1(..)
  , Term2(..)
  , Axiom(..)
  , toUnicode
  ) where

import Control.Monad (mfilter)

data Term1 = Factorial | Negate | Function String
  deriving (Show, Eq)
data Term2 = Sum | Product | Fraction | Exponent | Series String | Limit String
  deriving (Show, Eq)

data Term =
  Hole |
  Const Integer |
  Var String |
  Op1 Term1 Term |
  Op2 Term2 Term Term
  deriving (Show, Eq)

data Axiom = Axiom {
  description :: String,
  example :: (String, String),
  implementation :: Term -> Either Term Term
}

instance Eq Axiom where
  a == b = description a == description b

instance Show Axiom where
  show (Axiom { description = d }) = d

toUnicode Hole             = "_"
toUnicode (Const a)        = show a
toUnicode (Var a)          = a
toUnicode t@(Op2 Sum a (Op1 Negate b))      = maybeBrackets t a <> " - " <> maybeBrackets t b
toUnicode t@(Op2 Sum a b) = maybeBrackets t a <> " + " <> maybeBrackets t b
toUnicode t@(Op2 Product a b)  =
  let operator = case (a, b) of
                   (_, Const{}) -> "⋅"
                   (_, Op1 Negate _) -> "⋅"
                   _            -> ""
  in maybeBrackets t a <> operator  <> maybeBrackets t b

toUnicode t@(Op1 Factorial a)  = maybeBrackets t a <> "!"
toUnicode t@(Op2 Fraction a b) = maybeBrackets t a <> "/" <> maybeBrackets t b
toUnicode t@(Op2 Exponent a b) = maybeBrackets t a <> "^" <> maybeBrackets t b
toUnicode t@(Op1 Negate a) = "-" <> maybeBrackets t a
toUnicode t@(Op1 (Function name) a) = name <> "(" <> toUnicode a <> ")"
toUnicode (Op2 (Series v) i t)   =
  "Σ[" <> v <> " = " <> toUnicode i <> "](" <> toUnicode t <> ")"
toUnicode (Op2 (Limit v) i t)   =
  "lim[" <> v <> " → " <> toUnicode i <> "](" <> toUnicode t <> ")"

maybeBrackets parent child = let inner = toUnicode child in
  if precedence child < precedence parent then
    "(" <> inner <> ")"
  else
    inner

precedence (Op1 Factorial _) = 40
precedence (Op2 Exponent _ _) = 30
precedence (Op2 Product _ _) = 20
precedence (Op2 Fraction _ _) = 20
precedence (Op2 Sum _ _) = 10
precedence _ = 99

-- TODO: Use -> for arrow
toAscii :: Term -> String
toAscii = replace 'Σ' 'S' . replace '⋅' '*' . replace '→' '>' . toUnicode

-- TODO: replace with Text package
replace a b = map $ maybe b id . mfilter (/= a) . Just
