module Main where

import Control.Monad (replicateM)
import System.Random (randomRIO)

data Altitude = Ground | Low | High
  deriving (Eq, Ord)

newtype Bird = Bird {birdAltitude :: Altitude}

data Glider a = Glider
  {gliderAltitude :: Altitude,
   gliderPilot :: a}

data Jet a b = Jet
  {jetAltitude :: Altitude,
   jetPilot :: a,
   jetFuel :: b}

class Flying a where
  capable :: Altitude -> a -> Bool
  fly :: Altitude -> a -> a

instance Flying Bird where
  capable _ _ = True
  fly a' b
    | capable a' b = b {birdAltitude = a'}
    | otherwise = b

instance Flying (Glider a) where
  capable a' g @ Glider {gliderAltitude = a} = a' < a
  fly a' g
    | capable a' g = g {gliderAltitude = a'}
    | otherwise = g

instance Integral b => Flying (Jet a b) where
  capable a' j @ Jet
    {jetAltitude = a,
     jetFuel = f} = a' < a || f > 0
  fly a' j @ Jet {jetFuel = f}
    | capable a' j = j
        {jetAltitude = a',
         jetFuel = f - 1}
    | otherwise = j

simulate :: Flying a => [Altitude] -> a -> a
simulate as f = foldr fly f as

describe :: Altitude -> String
describe Ground = "the ground"
describe Low = "a low altitude"
describe High = "a high altitude"

main :: IO ()
main =
  do a : as <- replicateM 3 $ (!!) [Ground, Low, High] <$> randomRIO (0, 2)
     f <- randomRIO (0, 2 :: Int)
     let b = Bird a
         g = Glider a "Errington"
         j = Jet a "Sayer" f
         b' = simulate as b
         g' = simulate as g
         j' = simulate as j
     putStrLn $ "A bird moved from " ++
       describe (birdAltitude b) ++ " to " ++
       describe (birdAltitude b') ++ "."
     putStrLn $ "A glider piloted by " ++
       gliderPilot g ++ " moved from " ++
       describe (gliderAltitude g) ++ " to " ++
       describe (gliderAltitude g') ++ "."
     putStrLn $ "A jet piloted by " ++
       jetPilot j ++ " with " ++
       show (jetFuel j) ++ " units of fuel moved from " ++
       describe (jetAltitude j) ++ " to " ++
       describe (jetAltitude j') ++ " by using " ++
       show (jetFuel j - jetFuel j') ++ " units of fuel."
