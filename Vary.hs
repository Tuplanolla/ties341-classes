{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module Main where

class Vary v where
  vary :: String -> v

instance Vary v => Vary (String -> v) where
  vary :: String -> String -> v
  vary x y = vary $ x ++ ' ' : y

instance Vary String where
  vary :: String -> String
  vary = id

instance Vary (IO a) where
  vary :: String -> IO a
  vary x = putStrLn x >> return undefined

main :: IO ()
main =
  do putStrLn $ vary "yes"
     putStrLn $ vary "yes" "no"
     vary "yes" "no" "maybe"
