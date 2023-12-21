{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Axiomator.Types
import Axiomator.Axioms
import Axiomator.Parser

main :: IO ()
main = defaultMain tests

testValidExample a =
  let
    f = implementation a
    d = description a
    (input, expected) = example a
  in
    testCase (d ++ ", " ++ input ++ " -> " ++ expected) $
      Right (parseUnsafe expected) @=? f (parseUnsafe input)

testApply a input expected =
  let
    f = implementation a . parseUnsafe
  in
    testCase (input ++ " -> " ++ expected) $
      Right (parseUnsafe expected) @=? f input

axiomsToTest =
  [ axiomCommuteSum
  , axiomAssociateSum
  , axiomCommuteProduct
  , axiomAssociateProduct
  , axiomSumConst
  , axiomMultiplyConst
  , axiomFactorialConst
  ]

tests :: TestTree
tests = testGroup "Axioms"
  [ testGroup "Examples" $ map testValidExample axiomsToTest
  , testGroup "Associate Sum" $
    [ testApply axiomAssociateSum "a + (b + c)" "(a + b) + c"
    , testApply axiomAssociateSum "(a + b) + c" "a + (b + c)"
    ]
  , testGroup "Associate Product" $
    [ testApply axiomAssociateProduct "a(bc)" "(ab)c"
    , testApply axiomAssociateProduct "(ab)c" "a(bc)"
    , testApply axiomAssociateProduct "(ab)/c" "a(b/c)"
    ]
  , testGroup "Multiply Constant" $
    [ testApply axiomMultiplyConst "2^3" "8"
    ]
  ]
