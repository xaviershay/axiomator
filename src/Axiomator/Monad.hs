{-# LANGUAGE OverloadedStrings #-}

module Axiomator.Monad where

import Axiomator.Axioms
import Axiomator.Types

import Control.Arrow (first)
import Control.Monad (msum, forM_)
import Control.Monad.RWS.Strict (RWS, tell, put, runRWS, get)
import Data.List (nub)

data Env = Env Term deriving (Show)
type Log = [(Term, Axiom)]
type AppMonad = RWS () Log Env


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

focus :: Term -> AppMonad () -> AppMonad ()
focus t m = do
  Env oldT <- get

  case locate t oldT of
    Just (t', cs) -> do
      let (newT, log) = runApp (Env t') m

      put . Env $ goRoot (newT, cs)
      tell $ map (first (\nt -> goRoot (nt, cs))) log
    Nothing -> error $ "Could not focus:\n  " <> toUnicode t <> "\n  " <> show t <> "\n\n  in:\n" <> toUnicode oldT <> "\n  " <> show oldT

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
