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
import qualified Data.HashMap.Strict as M
import Data.List (nub)
import Control.Monad (msum, forM_)
import Control.Monad.RWS.Strict (RWS, tell, put, runRWS, get)

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

locate :: Term -> Term -> Maybe Zipper
locate needle haystack = locate' needle (haystack, [])
  where
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
type AppMonad = RWS () Log Env
-- type AppEff effs = Members '[ Writer Log, State Env ] effs

ignoreError :: Either a a -> a
ignoreError (Left x) = x
ignoreError (Right x) = x

apply :: Axiom -> AppMonad ()
apply axiom = do
  Env t <- get

  case (implementation axiom) t of
    Right t' -> do
      tell [(t', axiom)]
      put (Env t')
    Left t' -> do
      error $ "couldn't apply "
        <> description axiom
        <> " to: \n  "
        <> toUnicode t'
        <> "\n  "
        <> show t'
        <> "\nfull term is: \n  "
        <> show t

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
    putStr lhs
    putStr " = "
    putStrLn rhs

runApp :: Env -> AppMonad a -> (Term, Log)
runApp env m = do
  let (_, Env t, log) = runRWS m () env

  (t, log)

initial :: Term -> AppMonad ()
initial t = do
  tell [(t, axiomIdentity)]
  put (Env t)

focus :: Term -> AppMonad () -> AppMonad ()
focus t m = do
  Env oldT <- get

  case locate t oldT of
    Just (t', cs) -> do
      let (newT, log) = runApp (Env t') m

      put . Env $ goRoot (newT, cs)
      tell $ map (first (\nt -> goRoot (nt, cs))) log
    Nothing -> error $ "Could not focus:\n  " <> toUnicode t <> "\n  " <> show t <> "\n\n  in:\n" <> toUnicode oldT <> "\n  " <> show oldT

runSolution :: AppMonad a -> IO ()
runSolution m = do
  let (_, log) = runApp (Env "_") m
  let usedAxioms = nub (map snd log)
  printAxioms usedAxioms
  putStrLn ""

  let paddingT = maximum $ map (length . toAscii . fst) log
  forM_ log $ \(t, axiom) -> do
    putStr (toAscii t)
    putStr $ replicate (paddingT - length (toAscii t)) ' '
    putStrLn $ " ; " <> description axiom

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

-- TODO: Handle variable aliasing properly for nested series
e_to t = (Op2 (Series "k") (Const 0) (Op2 Fraction (Op2 Exponent t (Var "k")) (Op1 Factorial (Var "k"))))
cos_x = parseUnsafe "S[m=0]((-1)^m*(x^(2*m))/(2*m)!)"
