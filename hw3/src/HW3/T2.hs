{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , unwrapList
  ) where

import           HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption =
  \case
    (Some x, Some y) -> Some (x, y)
    _ -> None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 y1, P x2 y2) = P (x1, x2) (y1, y2)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distAnnotated ::
     Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# e1, y :# e2) = (x, y) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept =
  \case
    (Success x, Success y) -> Success (x, y)
    (Error x, _) -> Error x
    (_, Error x) -> Error x

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised =
  \case
    (Low x, b) -> prioritizeByFirst x b
    (a, Low x) -> prioritizeBySecond x a
    (Medium x, b) -> prioritizeByFirst x b
    (a, Medium x) -> prioritizeBySecond x a
    (High x, b) -> prioritizeByFirst x b

prioritizeByFirst :: a -> Prioritised b -> Prioritised (a, b)
prioritizeByFirst x = mapPrioritised (x, )

prioritizeBySecond :: a -> Prioritised b -> Prioritised (b, a)
prioritizeBySecond x = mapPrioritised (, x)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> xs, y :> ys) = (x, y) :> distStream (xs, ys)

infixl 5 \++

(\++) :: List a -> List a -> List a
(\++) x y =
  case x of
    Nil       -> y
    (s :. sx) -> s :. (sx \++ y)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

unwrapList :: List (List a) -> List a
unwrapList (x :. xs) = x \++ unwrapList xs
unwrapList Nil       = Nil

distList :: (List a, List b) -> List (a, b)
distList =
  \case
    (x :. xs, y :. ys) ->
      unwrapList $ mapList (\xx -> mapList (xx, ) (y :. ys)) (x :. xs)
    _ -> Nil

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

wrapFun :: a -> Fun i a
wrapFun x = F (const x)
