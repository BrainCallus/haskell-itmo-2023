{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module T3.Simulation
  ( Config(..)
  , Cell(..)
  , CellState(..)
  , Comonad19Grid
  , simulate
  , isInfectedOrIll
  , decreasePeriod
  , prettyComonad19
  ) where

import           Control.Comonad               (Comonad (extend, extract))
import           System.Random                 (RandomGen (split), StdGen,
                                                mkStdGen, random)

import           Control.Lens                  (has, (&), (.~), (^.))
import           Data.Grid                     (Grid (..), gridGet, gridWrite,
                                                neighbourGetters)
import           Data.List                     (intercalate)
import           Data.ListZipper
import qualified Data.ListZipper               as LZ
import           Prettyprinter                 (Doc, annotate, pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)
import           T3.Cell
import           T3.Config

type Comonad19Grid = Grid Cell

prettyComonad19 :: Int -> Comonad19Grid -> Doc AnsiStyle
prettyComonad19 sz grid =
  mconcat
    $ intercalate [pretty "\n"]
    $ (\lz -> prettyCell <$> LZ.toList lz sz) <$> LZ.toList (unGrid grid) sz

prettyCell :: Cell -> Doc AnsiStyle
prettyCell cell =
  annotate (color (getStateColor (cell ^. cellState))) $ pretty (show cell)

getStateColor :: CellState -> Color
getStateColor =
  \case
    Healthy -> Green
    Infected _ -> Magenta
    Immune _ -> Blue
    Ill _ -> Red

-- goggle-eyed colors.. yummy
-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).
simulate :: Config -> Int -> [Comonad19Grid]
simulate conf seed =
  iterate (nextStage conf) (initializePandemy conf (mkStdGen seed))

initializePandemy :: Config -> StdGen -> Comonad19Grid
initializePandemy conf gen = do
  let clean =
        Grid
          (genericMove
             (produceCellZipper fst)
             (produceCellZipper snd)
             (initRow gen))
  gridWrite (Cell (Infected (conf ^. incubPer)) gen) clean

produceCellZipper ::
     ((StdGen, StdGen) -> StdGen) -> (ListZipper Cell -> ListZipper Cell)
produceCellZipper genFun =
  initRow . genFun . split . nextGenerator . (^. cellRand) . extract

initRow :: StdGen -> ListZipper Cell
initRow gen = genericMove (produceCell fst) (produceCell snd) (Cell Healthy gen)

produceCell :: ((StdGen, StdGen) -> StdGen) -> (Cell -> Cell)
produceCell genFun cell =
  Cell (cell ^. cellState) (genFun (split (cell ^. cellRand)))

nextStage :: Config -> Comonad19Grid -> Comonad19Grid
nextStage conf = extend (cellNextState conf)

cellNextState :: Config -> Comonad19Grid -> Cell
cellNextState config grid = do
  let cell = gridGet grid
  if has _Healthy (cell ^. cellState)
    then let (nGen, infect) = tryInfect (config ^. prob) grid (cell ^. cellRand)
          in Cell
               (if infect
                  then Infected (config ^. incubPer)
                  else Healthy)
               nGen
    else cell & cellState .~ statusProgress cell config

statusProgress :: Cell -> Config -> CellState
statusProgress cell config =
  if cell ^. stateDuration <= 0
    then nextStatus config (cell ^. cellState)
    else decreasePeriod cell

nextStatus :: Config -> CellState -> CellState
nextStatus config =
  \case
    Ill _ -> Immune (config ^. immDur)
    Infected _ -> Ill (config ^. illDur)
    _ -> Healthy

tryInfect :: Double -> Comonad19Grid -> StdGen -> (StdGen, Bool)
tryInfect probb grid = do
  let contagious =
        (length . filter isInfectedOrIll)
          ((\getter -> extract (getter grid) ^. cellState) <$> neighbourGetters)
  contactWithInfected contagious probb

contactWithInfected :: Int -> Double -> StdGen -> (StdGen, Bool)
contactWithInfected contagious probb gen =
  if contagious <= 0
    then (gen, False)
    else do
      let (pr, nGen) = random gen
      if pr < probb
        then (nGen, True)
        else contactWithInfected (contagious - 1) probb nGen

nextGenerator :: StdGen -> StdGen
nextGenerator gen' = snd $ split gen'
