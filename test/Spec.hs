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

testInvalid a input =
  let
    f = implementation a . parseUnsafe
  in
    testCase (input ++ " is invalid") $
      Left (parseUnsafe input) @=? f input

axiomsToTest =
  [ axiomIdentity
  , axiomCommuteSum
  , axiomAssociateSum
  , axiomCommuteProduct
  , axiomAssociateProduct
  , axiomDistribute
  , axiomDistributeLimit
  , axiomSumConst
  , axiomMultiplyConst
  , axiomFactorialConst
  , axiomIdentitySum
  , axiomIdentityProduct
  , axiomZeroProduct
  , axiomStepSeries
  , axiomSubstitute "x" "y"
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
  , testGroup "Distribute" $
    [ testApply axiomDistribute "ab+ac" "a(b+c)"
    , testApply axiomDistribute "a(b+c)" "ab+ac"
    ]
  , testGroup "Multiply Constant" $
    [ testApply axiomMultiplyConst "2^3" "8"
    ]
  , testGroup "Substitute" $
    [ testApply (axiomSubstitute "f(x)" "g(x)") "f(a+b)" "g(a+b)"
    , testApply (axiomSubstitute "f(x)" "g(x)") "f(f(a))" "g(f(a))"
    , testApply (axiomSubstitute "f(x)" "x+x") "f(a)" "a+a"
    , testApply (axiomSubstitute "x+y" "xy") "a+b" "ab"
    , testApply (axiomSubstitute "x+2" "x+1+1") "a+2" "a+1+1"
    , testApply (axiomSubstitute "x+x" "x*2") "(a+b)+(a+b)" "(a+b)*2"
    , testInvalid (axiomSubstitute "f(x)" "g(x)") "a+f(b)"
    , testInvalid (axiomSubstitute "x+x" "x*2") "(a+b)+(a+c)"
    ]
  ]
