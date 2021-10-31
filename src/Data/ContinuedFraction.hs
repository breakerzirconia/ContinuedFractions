module Data.ContinuedFraction
  ( -- * ContFraction data type declaration
    ContFraction

    -- * ContFraction construction
  , (+/)

    -- * Converter functions
  , toList
  , fromList
  , toRatio
  , fromRatio

    -- * Approximation
  , approx

    -- * Infinite ContFraction construction
  , cycleCFTerms
  , (+//)

    -- * Truncated continued fractions
  , cut
  , cutWhile

    -- * Infinite continued fraction generators
  , glenn
  ) where

import           Data.Ratio

infixr 5 :+/
data ContFraction a
  = Inf
  | a :+/ (ContFraction a)
  deriving Eq

infix 5 +/
(+/) :: a -> [a] -> ContFraction a
wh +/ fr = wh :+/ fromList fr

instance (Show a) => Show (ContFraction a) where
  show Inf          = error "Data.ContinuedFraction: Infinity"
  show (wh :+/ Inf) = show [wh]
  show cf = let (wh, fr) = break (== ',') . show $ toList cf
            in wh ++ ';' : tail fr

toList :: ContFraction a -> [a]
toList Inf         = []
toList (wh :+/ fr) = wh : toList fr

fromList :: [a] -> ContFraction a
fromList = foldr (:+/) Inf

toRatio :: Integral a => ContFraction a -> Ratio a
toRatio Inf          = error "Data.ContinuedFraction: Infinity"
toRatio (wh :+/ Inf) = fromIntegral wh
toRatio (wh :+/ fr)  = fromIntegral wh + (1 / toRatio fr)

fromRatio :: (Eq a, Integral a) => Ratio a -> ContFraction a
fromRatio r = go (numerator r) (denominator r)
  where
    go n d
      | d == 0 = Inf
      | otherwise = (n `quot` d) :+/ go d (n `rem` d)

approx :: (Integral a, Floating b) => ContFraction a -> b
approx = (\r -> fromIntegral (numerator r) / fromIntegral (denominator r)) . toRatio

instance Integral a => Num (ContFraction a) where
  cf1 + cf2 = fromRatio $ toRatio cf1 + toRatio cf2
  cf1 - cf2 = fromRatio $ toRatio cf1 - toRatio cf2
  cf1 * cf2 = fromRatio $ toRatio cf1 * toRatio cf2
  abs = fromRatio . abs . toRatio
  signum = fromRatio . signum . toRatio
  fromInteger x = fromInteger x +/ []

cycleCFTerms :: a -> [a] -> ContFraction a
cycleCFTerms wh frs = wh +/ cycle frs

infix 5 +//
(+//) :: a -> [a] -> ContFraction a
(+//) = cycleCFTerms

cut :: Int -> ContFraction a -> ContFraction a
cut i = fromList . take (max 1 $ i + 1) . toList

cutWhile :: (a -> Bool) -> ContFraction a -> ContFraction a
cutWhile f = fromList . (\lst -> case lst of
  []      -> error "Data.ContinuedFraction: Infinity"
  (h : t) -> h : takeWhile f t) . toList

glenn :: (Enum a, Num a) => a -> ContFraction a
glenn i = i +/ ([1 ..] >>= \k -> [1, k * i, 1])
