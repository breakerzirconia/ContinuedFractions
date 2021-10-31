module Data.ContinuedFraction
  ( -- * ContFraction data type declaration
    ContFraction

    -- * ContFraction construction
  , (+/)
  , fromRatio

    -- * Converter functions
  , toList
  , toRatio

    -- * Approximation
  , approx

    -- * Infinite ContFraction construction
  , cycleCFTerms
  , (+//)

    -- * Truncated continued fractions
  , cut
  , cutWhile

    -- * Infinite continued fraction generators & constants
  , glenn
  , bessel0
  , bessel1
  ) where

import           Data.Function
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

instance Functor ContFraction where
  fmap f Inf         = Inf
  fmap f (wh :+/ fr) = f wh :+/ fmap f fr

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

instance Integral a => Ord (ContFraction a) where
  compare = compare `on` toRatio

instance Integral a => Num (ContFraction a) where
  cf1 + cf2 = fromRatio $ toRatio cf1 + toRatio cf2
  cf1 - cf2 = fromRatio $ toRatio cf1 - toRatio cf2
  cf1 * cf2 = fromRatio $ toRatio cf1 * toRatio cf2
  abs = fromRatio . abs . toRatio
  signum = fromRatio . signum . toRatio
  fromInteger x = fromInteger x +/ []

instance Integral a => Fractional (ContFraction a) where
  cf1 / cf2 = fromRatio $ toRatio cf1 / toRatio cf2
  recip = fromRatio . recip . toRatio
  fromRational = fmap fromInteger . fromRatio

instance Integral a => Real (ContFraction a) where
  toRational = (\r -> toInteger (numerator r) % toInteger (denominator r)) . toRatio

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

bessel0 :: (Enum a, Num a) => ContFraction a
bessel0 = 0 +/ [1 ..]

bessel1 :: (Enum a, Num a) => ContFraction a
bessel1 = 1 +/ [2 ..]
