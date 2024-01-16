{-# LANGUAGE TemplateHaskell #-}

module T3.Config
  ( Config(..)
  , prob
  , incubPer
  , illDur
  , immDur
  ) where

--import Control.Exception (assert)
import           Control.Lens (makeLenses)

data Config = Config
  { _prob     :: Double
  , _incubPer :: Int
  , _illDur   :: Int
  , _immDur   :: Int
  }

$(makeLenses ''Config)
