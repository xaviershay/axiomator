{-# LANGUAGE OverloadedStrings #-}

module Main where

import Axiomator
import qualified Data.HashMap.Strict as M

-- main = defaultMain tests
main = runSolution solution

solutionSimple = do
  initial "a(b + c)"

  focus "b + c" $ apply axiomCommuteSum

solution = do
  -- Show that the derivative of sin(x) is cos(x)
  initial "lim[h->0]((sin(x+h)-sin(x))/h)"

  focus "sin(x+h)" $ apply (axiomSubstitute "sin(a+b)" "sin(a)cos(b) + sin(b)cos(a)")
  focus "_-sin(x)" $ apply axiomCommuteSum
  focus "-(sin(x))+_" $ apply axiomAssociateSum
  focus "-(sin(x))" $ apply axiomCommuteProduct
  focus "_/h" $ apply axiomDistribute
  focus "lim[h->_](_)" $ apply axiomDistributeLimit
  focus "sin(x)*_+sin(x)*_" $ apply axiomDistribute
  focus "sin(x)*_/_" $ apply axiomAssociateProduct
  focus "lim[h->_](sin(x)_)" $ apply axiomFactorLimit
  focus "sin(h)*_" $ apply axiomCommuteProduct
  focus "(cos(x)*_)/_" $ apply axiomAssociateProduct
  focus "lim[h->_](cos(x)_)" $ apply axiomFactorLimit
  focus "lim[h->_](sin(h)/_)" $ apply (axiomSubstitute "lim[a->0](sin(a)/a)" "1")
  focus "-(1)+cos(h)" $ apply axiomCommuteSum
  -- TODO: Fix structure match to handle negation properly
  -- _ is -1 in this next substitute
  focus "lim[h->_]((cos(h)-1)/_)" $ apply (axiomSubstitute "lim[a->0]((cos(a) + _)/a)" "0")
  focus "sin(x)_" $ apply axiomZeroProduct
  focus "cos(x)_" $ apply axiomIdentityProduct
  focus "0 + cos(x)" $ apply axiomCommuteSum
  focus "cos(x) + 0" $ apply axiomIdentitySum
