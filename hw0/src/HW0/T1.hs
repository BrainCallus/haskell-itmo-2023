{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->)(Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data (<->) a b =
  Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib =
  \case
    Left v -> (Left v, Left v)
    Right (a, b) -> (Right a, Right b)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso leftAssoc rightAssoc

leftAssoc :: Either a (Either b c) -> Either (Either a b) c
leftAssoc =
  \case
    Left a -> Left (Left a)
    Right (Left b) -> Left (Right b)
    Right (Right c) -> Right c

rightAssoc :: Either (Either a b) c -> Either a (Either b c)
rightAssoc =
  \case
    Left (Left a) -> Left a
    Left (Right b) -> Right (Left b)
    Right c -> Right (Right c)
