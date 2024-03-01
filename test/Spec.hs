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
  , axiomFactorLimit
  ]

tests :: TestTree
tests = testGroup "Axiomator"
  [ testGroup "Axiom Definitions"
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
    , testGroup "Commute Product" $
      [ testApply axiomCommuteProduct "-a" "a * -1"
      ]
    , testGroup "Distribute" $
      [ testApply axiomDistribute "ab+ac" "a(b+c)"
      , testApply axiomDistribute "a(b+c)" "ab+ac"
      , testApply axiomDistribute "(a+b)/c" "a/c+b/c"
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
      , testApply (axiomSubstitute "x+_" "x*2") "(a+b)+(a+b)" "(a+b)*2"
      , testInvalid (axiomSubstitute "f(x)" "g(x)") "a+f(b)"
      , testInvalid (axiomSubstitute "x+x" "x*2") "(a+b)+(a+c)"
      ]
    , testGroup "Factor limit" $
      [ testApply axiomFactorLimit "lim[h->0](xh)" "xlim[h->0](h)"
      , testInvalid axiomFactorLimit "lim[h->0](hx)"
      ]
    ]
  , testGroup "Simplifying expressions" $ validateAll simplify
    [ ("a + 0", "a")
    , ("0 + a", "a")
    , ("a*1", "a")
    , ("1*a", "a")
    , ("a/1", "a")
    , ("a^1", "a")
    , ("1+2", "3")
    , ("1+2+3", "6")
    , ("2*3", "6")
    , ("2*3*4", "24")
    , ("2*3+4", "10")
    , ("a^0", "1")
    , ("2^2", "4")
    , ("4/2", "2")
    , ("14/8", "7/4")
    , ("3-2", "1")
    , ("-1+2", "1")
    , ("-1-2", "-3")
    , ("3*(-2)", "-6")
    , ("-3*2", "-6")
    , ("-3*(-2)", "6")
    , ("-(-1)", "1")
    , ("-1*(-x)", "x")
    , ("-x*(-1)", "x")
    , ("-x/(-1)", "x")
    , ("sin(x)", "sin(x)")
    , ("S[h=0](h)", "S[h=0](h)")
    , ("lim[h->0](h)", "lim[h->0](h)")
    ]
  , let
      f (input, expected) = testCase (input <> " = " <> expected) $
        expected @=? toAscii (parseUnsafe input)
    in
    testGroup "toAscii (bracket reduction)" $ map f
      [ ("a+b", "a + b")
      , ("a+b+c", "a + b + c")
      , ("a+(bc)", "a + bc")
      , ("a*b+c)", "ab + c")
      , ("(a+b)*c", "(a + b)c")
      , ("abc", "abc")
      , ("a+b/c", "a + b/c")
      , ("(a+b)/c", "(a + b)/c")
      , ("a^2", "a^2")
      , ("(a+b)^(cd)", "(a + b)^(cd)")
      , ("(a+b)^c*d", "(a + b)^cd")
      , ("2a!", "2a!")
      , ("(2a)!", "(2a)!")
      ]
  ]

validate :: (Term -> Term) -> Term -> Term -> TestTree
validate f input expected =
  testCase (toUnicode input <> " = " <> toUnicode expected) $ (toUnicode $ f input) @?=(toUnicode $ expected)

validateAll :: (Term -> Term) -> [(Term, Term)] -> [TestTree]
validateAll f = map (uncurry $ validate f)
