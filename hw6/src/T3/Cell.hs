{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module T3.Cell
  ( CellState(..)
  , duration
  , Cell(..)
  , cellState
  , cellRand
  , isInfectedOrIll
  , decreasePeriod
  , stateDuration
  , _Healthy
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens        (Lens', Prism', has, lens, makeLenses,
                                      makePrisms, (%~), (&), (.~), (^.), (^?))
import           Data.Maybe          (fromMaybe)
import           System.Random       (StdGen)

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving (Eq)

$(makePrisms ''CellState)

$(makeLenses ''CellState)

instance Show CellState where
  show =
    \case
      Healthy -> "_"
      Infected _ -> "i"
      Ill _ -> "#"
      Immune _ -> "@"

duration :: Lens' CellState Int
duration = lens getter setter
  where
    getter :: CellState -> Int
    getter =
      \case
        Infected x -> x
        Ill x -> x
        Immune x -> x
        _ -> 0
    setter :: CellState -> Int -> CellState
    setter state x =
      fromMaybe
        Healthy
        (foldl (<|>) Nothing
           $ (\f -> f state x)
               <$> [maybeSet _Infected, maybeSet _Ill, maybeSet _Immune])

maybeSet :: Prism' CellState Int -> CellState -> Int -> Maybe CellState
maybeSet prism state x = do
  _n <- state ^? prism
  return (state & prism .~ x)

data Cell = Cell
  { _cellState :: CellState
  , _cellRand  :: StdGen
  }

$(makeLenses ''Cell)

stateDuration :: Lens' Cell Int
stateDuration = cellState . duration

instance Show Cell where
  show cell = show (cell ^. cellState)

isInfectedOrIll :: CellState -> Bool
isInfectedOrIll state = has _Infected state || has _Ill state

decreasePeriod :: Cell -> CellState
decreasePeriod cell = (cell & stateDuration %~ (+ (-1))) ^. cellState
--getCellPeriod :: Cell -> Int
--getCellPeriod cell =
--      if isn't _Healthy state
--      then undefined
--      else undefined
--      fromMaybe 0 $ state ^? _Infected <|> state ^? _Ill <|> state ^? _Immune
--      where state =  cell ^. cellState
  --fromMaybe Healthy
  --  $ foldl
  --      (<|>)
  --      Nothing
  --      [ maybeDecrease state _Infected
  --      , maybeDecrease state _Ill
  --      , maybeDecrease state _Immune
  --      ]
  --where
  --  state = cell ^. cellState
--maybeDecrease :: CellState -> Prism' CellState Int  -> Maybe CellState
--maybeDecrease s prism = do
--  _n <- s ^? prism
--  return (s & prism %~ (\x -> x - 1))
