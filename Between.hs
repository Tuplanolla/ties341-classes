{-# LANGUAGE FlexibleContexts, FlexibleInstances, PartialTypeSignatures #-}

module Main where

import Data.Function
import Text.Megaparsec
import Text.Megaparsec.String

class Between a where
  open :: a
  close :: a

newtype Paren a = Paren a
  deriving (Read, Show)

newtype Bracket a = Bracket a
  deriving (Read, Show)

instance Functor Paren where
  f `fmap` Paren x = Paren $ f x

instance Functor Bracket where
  f `fmap` Bracket x = Bracket $ f x

instance Applicative Paren where
  pure = Paren
  Paren f <*> Paren x = Paren $ f x

instance Applicative Bracket where
  pure = Bracket
  Bracket f <*> Bracket x = Bracket $ f x

instance Between (Paren String) where
  open = Paren "("
  close = Paren ")"

instance Between (Bracket String) where
  open = Bracket "["
  close = Bracket "]"

wrapParser ::
  (Applicative f, Between (f String)) =>
  f (Parser String)
wrapParser =
  let with open close = (between `on` string) open close (some alphaNumChar) in
      with <$> open <*> close

parseWrap ::
  (Applicative f, Between (f String)) =>
  String -> f (Either ParseError String)
parseWrap string =
  let run parser = runParser parser "" string in
      run <$> wrapParser

main :: IO ()
main =
  do print (parseWrap "(yes)" :: Paren _)
     print (parseWrap "[yes]" :: Bracket _)
