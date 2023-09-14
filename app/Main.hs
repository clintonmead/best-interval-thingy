{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import qualified Data.List as List

main :: IO ()
main = print . genTopIntervals $ intervals

intervals :: [IntervalAndElement Int Int]
intervals = 
  [
  (IntervalAndElement (Interval 21 22) 300),
  (IntervalAndElement (Interval 15 23) 200),
  (IntervalAndElement (Interval 20 23) 200)
  ]

data Interval pointT = Interval { start :: pointT, end :: pointT }
  deriving stock Show

data StartOrEnd = Start | End

data IntervalAndElement pointT elemT = 
  IntervalAndElement { interval :: Interval pointT, element :: elemT }

instance (Show pointT, Show elemT) => Show (IntervalAndElement pointT elemT) where
  show (IntervalAndElement (Interval start end) element) = 
    "( " ++ show start ++ ", " ++ show end ++ ", " ++ show element ++ ")"

data PointAndElement pointT elemT =
  PointAndElement { point :: pointT, element :: elemT, startOrEnd :: StartOrEnd }

data PointAndElementWithPrevious pointT elemT = 
  PointAndElementWithPrevious { pointAndElement :: PointAndElement pointT elemT, previousPoint :: pointT }

splitInterval :: IntervalAndElement pointT elemT -> [PointAndElement pointT elemT]
splitInterval (IntervalAndElement (Interval start end) element) =
  [PointAndElement start element Start, PointAndElement end element End]

splitIntervals :: Ord pointT => [IntervalAndElement pointT elemT] -> [PointAndElement pointT elemT]
splitIntervals = List.sortOn (\x -> x.point) . concatMap splitInterval

addPreviousPoint :: PointAndElement pointT elemT -> PointAndElement pointT elemT -> PointAndElementWithPrevious pointT elemT
addPreviousPoint currentPointAndElem previousPointAndElem = 
  PointAndElementWithPrevious currentPointAndElem previousPointAndElem.point

makeIntervalsWithPrevious :: [PointAndElement pointT elemT] -> [PointAndElementWithPrevious pointT elemT]
makeIntervalsWithPrevious l = zipWith addPreviousPoint (tail l) l

genTopIntervals :: forall pointT elemT. (Ord elemT, Ord pointT) => [IntervalAndElement pointT elemT] -> [IntervalAndElement pointT elemT]
genTopIntervals l =
  let
    l' :: [PointAndElement pointT elemT]
    l' = splitIntervals $ l
    firstElem :: elemT
    firstElem = (head l').element
    l'' :: [PointAndElementWithPrevious pointT elemT]
    l'' = makeIntervalsWithPrevious l'
  in
    tail $ fst <$> genTopIntervals' firstElem l''

genTopIntervals' :: forall pointT elemT. Ord elemT => elemT -> [PointAndElementWithPrevious pointT elemT] -> [(IntervalAndElement pointT elemT, MultiSet elemT)]
genTopIntervals' initialElement = scanl f (undefined {- the "tail" above means we don't look at this anyway -}, MultiSet.singleton initialElement) where
  f :: (IntervalAndElement pointT elemT, MultiSet elemT) -> PointAndElementWithPrevious pointT elemT -> (IntervalAndElement pointT elemT, MultiSet elemT)
  f (_, prevMultiSet) (PointAndElementWithPrevious (PointAndElement end elem startOrEnd) start) = 
    let 
      newMultiSet = case startOrEnd of
        Start -> MultiSet.insert elem prevMultiSet
        End -> MultiSet.delete elem prevMultiSet
      newIntervalAndElement = IntervalAndElement (Interval start end) (MultiSet.findMax prevMultiSet)
    in
      (newIntervalAndElement, newMultiSet)
