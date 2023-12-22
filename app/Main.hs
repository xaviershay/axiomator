{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Axiomator.Types
import Axiomator.Parser
import Axiomator.Axioms
import Control.Arrow (first)
import Control.Monad (mfilter)
import Data.String (IsString(..))
import Debug.Trace (trace, traceM)
import Data.List
import Data.Hashable
import GHC.Generics hiding (Infix, Prefix)
import qualified Data.Tuple
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Monoid ((<>))
import Control.Monad (msum, forM_)


-- import Control.Monad.Freer (Eff, Members, Member, run, runM)
-- import Control.Monad.Freer.Error (Error, throwError, runError)
-- import Control.Monad.Freer.State (State(..), get, gets, put, runState)
-- import Control.Monad.Freer.Writer (Writer(..), tell, runWriter)
import Test.Tasty
import Test.Tasty.HUnit






axiomFactor factor = Axiom {
  description = "Factor",
  example = (
    parseUnsafe "lim[h->x](ab)",
    parseUnsafe "a*lim[h->x](b)"
  ),
  implementation = f
}
  where
    f (Op2 limit@(Limit _) v inner@(Op2 Product _ _)) = Right $ Op2 Product factor (Op2 limit v (simplify . cancelTerm factor $ Op2 Fraction inner factor))
    f t = Left t

allAxioms =
  [ axiomCommuteSum
  , axiomAssociateSum
  , axiomIdentitySum
  , axiomDistribute
  , axiomStepSeries
  , axiomSumConst
  , axiomMultiplyConst
  , axiomFactorialConst
  ]

p = parseUnsafe
ps = putStrLn . toAscii

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

distribute t (Op2 Product a (Op2 Sum b c)) =
  let x = cancelTerm t $ Op2 Fraction a t in

  Op2 Product x $
    Op2 Sum
      (Op2 Product t b)
      (Op2 Product t c)

undistribute t (Op2 Sum a b) =
  Op2 Product t $
    Op2 Sum
      (cancelTerm t $ Op2 Fraction a t)
      (cancelTerm t $ Op2 Fraction b t)

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

filterZip :: (Term -> Bool) -> Zipper -> [Zipper]
filterZip f (Hole, _) = []
filterZip f z@(t, cs) = do
  let currentNode = if f t then [(t, cs)] else []
      lhs = filterZip f (goLeft z)
      rhs = filterZip f (goRight z)
    in currentNode ++ lhs ++ rhs

-- For each var
--   follow in t
--   assign value to var
--   need a map of var -> Term


locate :: Term -> Term -> Maybe Zipper
locate needle haystack = locate' needle (haystack, [])

locate' :: Term -> Zipper -> Maybe Zipper
locate' Hole z = Just z
locate' _ (Hole, _) = Nothing
locate' a z@(b, _) | a `termEqual` b = Just z
locate' a z = msum . map (locate' a) . goDown $ z

termEqual :: Term -> Term -> Bool
termEqual Hole _ = True
termEqual _ Hole = True
termEqual (Op1 op1 a) (Op1 op2 c) = op1 == op2 && a `termEqual` c
termEqual (Op2 op1 a b) (Op2 op2 c d) = op1 == op2 && a `termEqual` c && b `termEqual` d
termEqual (Var a) (Var c) = a == c
termEqual (Const a) (Const c) = a == c
termEqual (Const a) (Op1 Negate (Const c)) = a == -c
termEqual (Op1 Negate (Const a)) (Const c) = a == -c
termEqual _ _ = False

data Env = Env Term deriving (Show)

type Log = [(Term, Axiom)]
-- type AppEff effs = Members '[ Writer Log, State Env ] effs

ignoreError :: Either a a -> a
ignoreError (Left x) = x
ignoreError (Right x) = x

-- apply :: AppEff effs => Axiom -> Eff effs ()
-- apply axiom = do
--   Env t <- get
-- 
--   case (implementation axiom) t of
--     Right t' -> do
--       tell [(t', axiom)]
--       put (Env t')
--     Left t' -> do
--       error $ "couldn't apply " <> description axiom <> " to " <> toUnicode t' <> " (full term is " <> toUnicode t <> ")"


-- highlightTerms m = do
--   Env t <- get
-- 
--   let ms = filterZip m (t, [])
-- 
--   traceM "====="
--   forM_ ms $ \(t',_) -> do
--     traceM . toAscii $ t'
--     traceM "---"

-- TODO: Handle variable aliasing properly for nested series
e_to t = (Op2 (Series "k") (Const 0) (Op2 Fraction (Op2 Exponent t (Var "k")) (Op1 Factorial (Var "k"))))
cos_x = parseUnsafe "S[m=0]((-1)^m*(x^(2*m))/(2*m)!)"

printAxioms axioms = do
  let paddingIndex = length (show $ length axioms)
  let paddingDesc = maximum . map (length . description) $ axioms
  forM_ (zip axioms [1..]) $ \(axiom, i) -> do
    putStr (show i)
    putStr ". "
    putStr $ replicate (paddingIndex - length (show i)) ' '
    putStr (description axiom)
    putStr ": "
    putStr $ replicate (paddingDesc - length (description axiom)) ' '
    let (lhs, rhs) = example axiom
    putStr $ toAscii lhs
    putStr " = "
    putStrLn $ toAscii rhs

-- type AllEffs = '[ Writer Log, State Env ]
-- runApp :: Env -> Eff AllEffs a -> (Term, Log)
-- runApp env m = do
--   let ((_, log), (Env t)) = run . runState env . runWriter $ m
-- 
--   (t, log)
-- 
-- initial :: Term -> Eff AllEffs ()
-- initial t = do
--   tell [(t, axiomIdentity)]
--   put (Env t)

-- focus :: Term -> Eff AllEffs () -> Eff AllEffs ()
-- focus t m = do
--   Env oldT <- get
-- 
--   case locate t oldT of
--     Just (t', cs) -> do
--       let (newT, log) = runApp (Env t') m
-- 
--       put . Env $ goRoot (newT, cs)
--       tell $ map (first (\nt -> goRoot (nt, cs))) log
--     Nothing -> error $ "Could not focus:\n  " <> toUnicode t <> " in\n  " <> toUnicode oldT

-- runSolution :: Eff '[ Writer Log, State Env] a -> IO ()
-- runSolution m = do
--   let (_, log) = runApp (Env "_") m
--   let usedAxioms = nub (map snd log)
--   printAxioms usedAxioms
--   putStrLn ""
-- 
--   let paddingT = maximum $ map (length . toAscii . fst) log
--   forM_ log $ \(t, axiom) -> do
--     putStr (toAscii t)
--     putStr $ replicate (paddingT - length (toAscii t)) ' '
--     putStrLn $ " ; " <> description axiom

-- runProcess t m = do
--   let (_, log) = runApp (Env t) m
--   let usedAxioms = nub (map snd log)
--   printAxioms usedAxioms
--   putStrLn ""
-- 
--   let paddingT = maximum $ map (length . toAscii . fst) log
--   putStrLn . toAscii $ t
--   forM_ log $ \(t, axiom) -> do
--     putStr (toAscii t)
--     putStr $ replicate (paddingT - length (toAscii t)) ' '
--     putStrLn $ " ; " <> description axiom

--main = body
main = defaultMain tests
-- main = runSolution solution
--main = putStrLn $ show testF

-- solution2 = do
--   initial "a(b + c)"
-- 
--   apply axiomDistribute
--   apply axiomCommuteSum
-- 
-- solution = do
--   initial "lim[h->0]((sin(x+h)-sin(x))/h)"
-- 
--   focus "sin(x+h)" $ apply (axiomSubstitute "sin(a+b)" "sin(a)cos(b) + sin(b)cos(a)")
--   focus "_-sin(x)" $ apply axiomCommuteSum
--   focus "-(sin(x))+_" $ apply axiomAssociateSum
--   focus "-(sin(x))+_" $ apply axiomDistribute
--   focus "_/h" $ apply axiomDistribute
--   focus "lim[h->_](_)" $ apply axiomDistributeLimit
--   focus "(sin(x)*_)/_" $ apply axiomAssociateProduct
--   focus "lim[h->_](sin(x)_)" $ apply (axiomFactor "sin(x)")
--   focus "sin(h)*_" $ apply axiomCommuteProduct
--   focus "(cos(x)*_)/_" $ apply axiomAssociateProduct
--   focus "lim[h->_](cos(x)_)" $ apply (axiomFactor "cos(x)")
--   focus "lim[h->_](sin(h)/_)" $ apply (axiomSubstitute "lim[a->0](sin(a)/a)" "1")
--   focus "-(1)+cos(h)" $ apply axiomCommuteSum
--   focus "lim[h->_]((cos(h)-1)/_)" $ apply (axiomSubstitute "lim[a->0]((cos(a)-1)/a)" "0")
--   focus "sin(x)_" $ apply axiomZeroProduct
--   focus "cos(x)_" $ apply axiomIdentityProduct

validate :: (Term -> Term) -> Term -> Term -> TestTree
validate f input expected =
  testCase (toUnicode input <> " = " <> toUnicode expected) $ (toAscii $ f input) @?=(toAscii $ expected)

validateAll :: TestName -> (Term -> Term) -> [(Term, Term)] -> TestTree
validateAll name f = testGroup name . map (uncurry $ validate f)

toAsciiTests =
  testGroup "toAscii (bracket reduction)" . map f $
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

  where
    f :: (String, String) -> TestTree
    f (input, expected) = testCase (input <> " = " <> expected) $ expected @=? toAscii (parseUnsafe input)

tests = testGroup "Axioms"
  [ toAsciiTests
  , validateAll "simplify" simplify
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
  , validateAll "distribute \"a\"" (simplify . distribute "a") $
      [ ("a(b+c)", "ab+ac")
      , ("2a(b+c)", "2(ab+ac)")
      ]
  , validateAll "undistribute \"a\"" (simplify . undistribute "a")
      [ ("ab+ac", "a(b+c)")
      , ("ba+ac", "a(b+c)")
      , ("ab+ca", "a(b+c)")
      , ("ba+ca", "a(b+c)")
      , ("ab+a", "a(b+1)")
      , ("ba+a", "a(b+1)")
      , ("a+ab", "a(1+b)")
      , ("a+ba", "a(1+b)")
      , ("b+c", "a*(b/a+c/a)")
      ]
    , testGroup "cancelTerm (exponents)"
      [ validate (simplify . cancelTerm "x^1") "x^2/x^1" "x"
      , validate (simplify . cancelTerm "x") "x^2/x^1" "x"
      , validate (simplify . cancelTerm "x") "x^2/x" "x"
      , validate (simplify . cancelTerm "x") "x/x^1" "1"
      , validate (simplify . cancelTerm "x") "2x/x" "2"
      ]
    , testGroup "random"
      [ testCase "functions not parsed as products of variables" $
          Left "sin(x)" @=? (implementation axiomCommuteProduct) "sin(x)"
      ]
  ]
