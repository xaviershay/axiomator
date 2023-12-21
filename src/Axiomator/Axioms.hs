{-# LANGUAGE OverloadedStrings #-}

module Axiomator.Axioms where

import Axiomator.Types
import Axiomator.Parser ()

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
    f (Op2 Product a b) = Right (Op2 Product b a)
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
    --f         (Op2 Fraction (Op2 Product a b) c)
    --  = Right (Op2 Product a (Op2 Fraction b c))
    f t
      = Left t
