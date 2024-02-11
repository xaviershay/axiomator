module Axiomator.Types
  ( Term(..)
  , Term1(..)
  , Term2(..)
  , Axiom(..)
  , Zipper
  , Crumb(..)
  , mkZipper
  , goDown
  , goLeft
  , goRight
  , goRoot
  , allZips
  , toAscii
  , toUnicode
  , walk
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

data Crumb =
    LeftCrumb Term
  | RightCrumb Term
  deriving (Show)

type Crumbs = [Crumb]
type Zipper = (Term, Crumbs)

mkZipper t = (t, [])

goLeft :: Zipper -> Zipper
goLeft (Op2 op l r, cs) = (l, LeftCrumb (Op2 op Hole r):cs)
goLeft (Op1 op l, cs) = (l, LeftCrumb (Op1 op Hole):cs)
goLeft z = error $ show z
--goLeft (t, cs) = (Hole, cs)

goRight :: Zipper -> Zipper
goRight (Op2 op l r, cs) = (r, RightCrumb (Op2 op l Hole):cs)
goRight (Op1 Factorial t, cs) = (t, RightCrumb (Op1 Factorial Hole):cs)
goRight z = error $ show z
--goRight (t, cs) = (Hole, cs)

goUp :: Zipper -> Zipper
goUp (t, LeftCrumb (Op2 op _ r):cs) = (Op2 op t r, cs)
goUp (t, RightCrumb (Op2 op l _):cs) = (Op2 op l t, cs)

goRoot :: Zipper -> Term
goRoot (t, []) = t
goRoot z = goRoot . goUp $ z

goDown :: Zipper -> [Zipper]
goDown (Op2 op l r, cs) = [(l, LeftCrumb (Op2 op Hole r):cs), (r, RightCrumb (Op2 op l Hole):cs)]
goDown (Op1 op l, cs) = [(l, LeftCrumb (Op1 op Hole):cs)]
goDown _ = []

allZips :: Term -> [Zipper]
allZips t = allZips' (t, [])
  where
    allZips' :: Zipper -> [Zipper]
    allZips' z = let zs = goDown z in z:concatMap allZips' zs

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

walk :: (Term -> Term) -> Term -> Term
walk f (Op1 op t) = f (Op1 op (walk f t))
walk f (Op2 op a b) = f (Op2 op (walk f a) (walk f b))
walk f t = f t
