module Tasks.T7
  ( moving
  , AmountOfElementsShouldBePositive(..)
  ) where

import           Control.Monad.State

newtype AmountOfElementsShouldBePositive =
  AmountOfElementsShouldBePositive String
  deriving (Show)

castDouble :: (Integral a) => a -> Double
castDouble = fromIntegral

takeLastN :: Int -> [a] -> [a]
takeLastN n list = drop (length list - n) list

moving :: Int -> [Double] -> Either AmountOfElementsShouldBePositive [Double]
moving n list
  | n <= 0 =
    Left
      (AmountOfElementsShouldBePositive ("Can't take " ++ show n ++ " elements"))
  | otherwise =
    Right
      $ evalState
          (mapM
             (\element -> do
                prev <- get
                let lastN = takeLastN n (prev ++ [element])
                let avg = sum lastN / castDouble (length lastN)
                put lastN
                return avg)
             list)
          []
