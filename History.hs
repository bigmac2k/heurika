{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

{- Overengineered history functionality
- Manages nodes of two kinds: Normal and StackBack
- Normal is just a normal slide advancement through the up and down buttons
- StackBack additionally indicates that the presenter used the right or left buttons
- The idea is that the presenter can go back to where he left the normal path quickly

- Overengineered because (as often) optimized for laaaarge N where it is probably small
- Uses fingertrees with tricky Monoid and two compatible orderings to be able to
- jump to any nth next Stackback in log(n).
- also allows cutting of the history in log(n).
-}
module History (
     History
    ,empty
    ,addHist
    ,addStack
    ,histPop
    ,stackPop
    ,stackBackN
    ,histBackN
    )  where

import Data.Monoid
import Data.FingerTree hiding (empty)
import qualified Data.FingerTree

data HistItem = Hist Int | Stack Int deriving (Show)
type History = FingerTree Measure HistItem
data Measure = Measure (Int, Int) deriving (Show)

instance Monoid Measure where
    mempty = Measure (0, 0)
    mappend (Measure (x1, y1)) (Measure (x2, y2)) = Measure (x1 + x2, y1 + y2)

instance Measured Measure HistItem where
    measure (Hist _) = Measure (1, 0)
    measure (Stack _) = Measure (1, 1)

addHist, addStack :: Int -> History -> History
addHist x xs = Hist x <| xs
addStack x xs = Stack x <| xs
stackBackN, histBackN :: Int -> History -> (Maybe Int, History)
stackBackN n hist = case viewl nHist of
                         EmptyL -> (Nothing, hist)
                         Hist x :< xs -> (Just x, xs) -- should not be required
                         Stack x :< xs -> (Just x, xs)
    where nHist = dropUntil (\(Measure (_, y)) -> y >= n) hist
histBackN n hist = case viewl nHist of
                        EmptyL -> (Nothing, hist)
                        Hist x :< xs -> (Just x, xs)
                        Stack x :< xs -> (Just x, xs)
    where nHist = dropUntil (\(Measure (x, _)) -> x >= n) hist
histPop, stackPop :: History -> (Maybe Int, History)
histPop = histBackN 1 -- head viewl -> :<
stackPop = stackBackN 1

empty :: History
empty = Data.FingerTree.empty

hist = fromList $ map (\x -> if x `mod` 5 == 0 then Stack x else Hist x) [1..100]
