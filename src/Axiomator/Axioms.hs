{-# LANGUAGE OverloadedStrings #-}

module Axiomator.Axioms where

import Debug.Trace

import Data.List (nub, groupBy)

import Axiomator.Types
import Axiomator.Parser ()

instantiateVariable :: String -> Term -> Term -> Term
instantiateVariable name value (Var vname) | name == vname = value
instantiateVariable _ _ t = t

axiomCommuteSum = Axiom {
  description = "Commutative law for addition",
  example = ("a+b", "b+a"),
  implementation = f
}
  where
    f (Op2 Sum a b) = Right (Op2 Sum b a)
    f t = Left t

axiomAssociateSum = Axiom {
  description = "Associative law for addition",
  example = ("a+(b+c)", "(a+b)+c"),
  implementation = f
}
  where
    f (Op2 Sum (Op2 Sum a b) c) = Right (Op2 Sum a (Op2 Sum b c))
    f (Op2 Sum a (Op2 Sum b c)) = Right (Op2 Sum (Op2 Sum a b) c)
    f t = Left t

axiomCommuteProduct = Axiom {
  description = "Commutative law for multiplication",
  example = ("ab", "ba"),
  implementation = f
}
  where
    f (Op2 Product a b) = Right $ Op2 Product b a
    f (Op1 Negate t) = Right $ Op2 Product t (Op1 Negate (Const 1))
    f t = Left t

axiomAssociateProduct = Axiom {
  description = "Associative law for multiplication",
  example = (
    "a(bc)",
    "(ab)c"
  ),
  implementation = f
}
  where
    f         (Op2 Product (Op2 Product a b) c)
      = Right (Op2 Product a (Op2 Product b c))
    f         (Op2 Product a (Op2 Product b c))
      = Right (Op2 Product (Op2 Product a b) c)
    f         (Op2 Fraction (Op2 Product a b) c)
      = Right (Op2 Product a (Op2 Fraction b c))
    f t
      = Left t

axiomSumConst = Axiom {
  description = "Sum constants",
  example = ("1+2", "3"),
  implementation = f
}
  where
    f (Op2 Sum (Const a) (Const b)) = Right (Const $ a + b)
    f t = Left t

axiomMultiplyConst = Axiom {
  description = "Multiply constants",
  example = ("2*3", "6"),
  implementation = f
}
  where
    f (Op2 Product (Const a) (Const b)) = Right (Const $ a * b)
    f (Op2 Exponent (Const a) (Const b)) = Right (Const $ a ^ b)
    f t = Left t

axiomFactorialConst = Axiom {
  description = "Factorial constants",
  example = ("3!", "6"),
  implementation = f
}
  where
    f (Op1 Factorial (Const x)) = Right . Const $ factorial x
    f t = Left t

    factorial 0 = 1
    factorial x = x * factorial (x-1)

axiomIdentitySum = Axiom {
  description = "Additive identity",
  example = ("a+0", "a"),
  implementation = f
}
  where
    f (Op2 Sum t (Const 0)) = Right t
    f t = Left t

axiomIdentityProduct = Axiom {
  description = "Multiplicative identity",
  example = ("a*1", "a"),
  implementation = f
}
  where
    f (Op2 Product t (Const 1)) = Right t
    --f t = Right $ Op2 Product t (Const 1)
    --f (Op2 Fraction t (Const 1)) = Right t
    f t = Left t

axiomZeroProduct = Axiom {
  description = "Multiplicative zero",
  example = (
    "a*0",
    "0"
  ),
  implementation = f
}
  where
    f (Op2 Product t (Const 0)) = Right (Const 0)
    f t = Left t

axiomIdentity = Axiom {
  description = "Identity",
  example = ("a", "a"),
  implementation = f
}
  where
    f t = Right t

axiomDistribute = Axiom {
  description = "Distributive law",
  example = ("a*(b+c)","ab+ac"),
  implementation = f
}
  where
    -- ab+ac -> a*(b+c)
    f (Op2 Sum (Op2 Product p1l p1r) (Op2 Product p2l p2r))
      | p1l == p2l = Right $ Op2 Product p1l (Op2 Sum p1r p2r)
    ---- a(b+c) -> ab+ac
    f (Op2 Product pl (Op2 Sum sl sr)) =
      Right $ Op2 Sum (Op2 Product pl sl) (Op2 Product pl sr)
    ---- -x+ab -> x*-1+ab
    --f (Op2 Sum (Op1 Negate l) p@(Op2 Product _ _)) = f (Op2 Sum (Op2 Product l (Const $ -1)) p)
    ---- x+ab -> x*1+ab
    --f (Op2 Sum l p@(Op2 Product _ _)) = f (Op2 Sum (Op2 Product l (Const 1)) p)
    f (Op2 Fraction (Op2 Sum l r) d) = Right $ Op2 Sum (Op2 Fraction l d) (Op2 Fraction r d)
    f t = Left t

axiomDistributeLimit = Axiom {
  description = "Distributive law for limits",
  example = ("lim[h->x](a+b)", "lim[h->x](a) + lim[h->x](b)"),
  implementation = f
}
  where
    f (Op2 limit@(Limit _) v (Op2 Sum l r)) =
      Right $ Op2 Sum (Op2 limit v l) (Op2 limit v r)
    f t = Left t

axiomStepSeries = Axiom {
  description = "Step series",
  example = ("S[k=a](k)", "a + S[k=a+1](k)"),
  implementation = f
}
  where
    f (Op2 (Series v) i t) = Right $
      Op2 Sum
        (walk (instantiateVariable v i) t)
        (Op2 (Series v) (Op2 Sum i (Const 1)) t)
    f t = Left t

axiomSubstitute pattern replacement = Axiom {
  description =
    "Identity: " ++ toUnicode pattern ++ " = " ++ toUnicode replacement,
  example = (toUnicode pattern, toUnicode replacement),
  implementation = f
}
  where
    f t =
      if structureMatch pattern t then
        -- Validate that all instances of a variable are equivalent
        let
          variableName = fst
          vars = groupBy
                   (\v1 v2 -> variableName v1 == variableName v2)
                   (identifyVars pattern t)
        in
          if all ((== 1) . length . nub . map varAndValue) vars then
            Right $ replaceVars (identifyVars pattern t) replacement
          else
            Left t
      else
        Left t

    varAndValue (var, (term, _)) = (var, term)

    identifyVars :: Term -> Term -> [(String, Zipper)]
    identifyVars pattern t = map f . locateVars $ pattern
      where
        f z@(Var x, _) = (x, follow z (mkZipper t))

    replaceVars :: [(String, Zipper)] -> Term -> Term
    replaceVars vs t = foldl go t vs
      where
        go :: Term -> (String, Zipper) -> Term
        go t' (v, (rt, _)) = walk (instantiateVariable v rt) t'

    -- Note: Assumes structure is identical (which it should be by
    -- pre-condition)
    follow :: Zipper -> Zipper -> Zipper
    follow (_, p) target = (foldr (.) id . extractF $ p) target
      where
        extractF [] = []
        extractF (LeftCrumb _:cs) = (goLeft:extractF cs)
        extractF (RightCrumb _:cs) = (goRight:extractF cs)

    structureMatch (Const i1) (Const i2) = i1 == i2
    structureMatch (Op1 _ t1) (Op1 _ t2) = structureMatch t1 t2
    structureMatch (Op2 _ t1l t1r) (Op2 _ t2l t2r) =
      structureMatch t1l t2l && structureMatch t1r t2r
    structureMatch (Var _) _ = True
    structureMatch Hole _ = True
    structureMatch _ _ = False

locateVars :: Term -> [Zipper]
locateVars = filter isVar . allZips
  where
    isVar (Var _, _) = True
    isVar _ = False

axiomFactorLimit = Axiom {
  description = "Factor constant from limit",
  example = (
    "lim[h->x](ab)",
    "a*lim[h->x](b)"
  ),
  implementation = f
}
  where
    f (Op2 limit@(Limit limitVar) v inner@(Op2 Product factor _))
      -- Ensure not trying to factor our limit variable
      | (not . any ((==) (Var limitVar) . fst)) . locateVars $ factor =

      Right $ Op2 Product
        factor
        (Op2 limit
          v
          (simplify . cancelTerm factor $ 
            Op2 Fraction inner factor))
    f t = Left t

cancelTerm :: Term -> Term -> Term
cancelTerm (Op2 Exponent x y) f@(Op2 Fraction (Op2 Exponent a b) (Op2 Exponent c d)) =
  case Op2 Fraction <$> numerator <*> denominator of
    Just x -> x
    Nothing -> f
  where
    numerator = if x == a then Just (Op2 Exponent a (Op2 Sum b (Op2 Product (Const (-1)) y))) else Nothing
    denominator = if x == c then Just (Op2 Exponent c (Op2 Sum d (Op2 Product (Const (-1)) y))) else Nothing

cancelTerm t f@(Op2 Fraction (Op2 Exponent _ _) (Op2 Exponent _ _)) = cancelTerm (Op2 Exponent t (Const 1)) f
cancelTerm t (Op2 Fraction lhs@(Op2 Exponent _ _) rhs) = cancelTerm t (Op2 Fraction lhs (Op2 Exponent rhs (Const 1)))
cancelTerm t (Op2 Fraction lhs rhs@(Op2 Exponent _ _)) = cancelTerm t (Op2 Fraction (Op2 Exponent lhs (Const 1)) rhs)
cancelTerm t f@(Op2 Fraction (Op2 Product a b) (Op2 Product c d)) =
    case Op2 Fraction <$> numerator <*> denominator of
      Just x -> x
      Nothing -> f
  where
    numerator =
      case (a, b) of
        (a, b) | a == t -> Just b
        (a, b) | b == t -> Just a
        _               -> Nothing
    denominator =
      case (c, d) of
        (c, d) | c == t -> Just d
        (c, d) | d == t -> Just c
        _               -> Nothing
cancelTerm t (Op2 Fraction l@(Op2 Product _ _) r) = cancelTerm t (Op2 Fraction l (Op2 Product r (Const 1)))
cancelTerm t (Op2 Fraction l r@(Op2 Product _ _)) = cancelTerm t (Op2 Fraction l (Op2 Product (Const 1) r))
cancelTerm t (Op2 Fraction l r) = cancelTerm t (Op2 Fraction (Op2 Product (Const 1) l) (Op2 Product (Const 1) r))

-- TODO: Resolve overlap between this and identify axioms
simplify t = walk f t
  where
    f (Op1 Negate (Const a)) = Const (-a)
    f (Op1 Negate (Op1 Negate a)) = a
    f (Op2 Sum (Const a) (Const b)) = Const $ a + b
    f (Op2 Product (Const a) (Const b)) = Const $ a * b
    f (Op2 Exponent a (Const 0)) = Const 1
    f (Op2 Exponent a (Const 1)) = a
    f (Op2 Exponent (Const a) (Const b)) = Const $ a ^ b
    f (Op2 Fraction a (Const 1)) = a
    f (Op2 Fraction a (Const (-1))) = f $ Op1 Negate a
    f t@(Op2 Fraction (Const a) (Const b)) =
      case gcd a b of
        1 -> t
        n -> simplify $ Op2 Fraction (Const $ a `div` n) (Const $ b `div` n)
    f (Op2 Product (Const 1) a) = a
    f (Op2 Product a (Const 1)) = a
    f (Op2 Product (Const (-1)) a) = f $ Op1 Negate a
    f (Op2 Product a (Const (-1))) = f $ Op1 Negate a
    f (Op2 Sum a (Const 0)) = a
    f (Op2 Sum (Const 0) a) = a
    f x = x

