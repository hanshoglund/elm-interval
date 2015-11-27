
module Interval where
{-|
A representation of numeric intervals (also known as *ranges*.)

These can be thoughts of as a pair of number, or as the set of all numbers that fall between them.

See also [Wikipedia on intervals](https://en.wikipedia.org/wiki/Interval_(mathematics)).

@docs Interval
@docs empty, full, isFull, isEmpty
@docs orderToInterval, ordersToIntervals, intervalToOrders
@docs map
@docs intersection, hull

-}

import Interval.EndPoint exposing (..)

{-| An open interval. -}
type Interval a = Interval { min : EndPoint a, max : EndPoint a }

{-| The interval containing no numbers (the empty set). -}
-- Identity for `hull`.
empty : Interval a
empty = Interval { min = PosInf, max = NegInf }

{-| The interval containing all numbers (the set of extended reals).
-- Identity for `intersection`. -}
full : Interval a
full = Interval { min = NegInf, max = PosInf }

{-| Is this the full interval? -}
isFull : Interval a -> Bool
isFull (Interval {min, max}) = min == NegInf && max == PosInf

{-| Is this the empty interval? -}
isEmpty : Interval a -> Bool
isEmpty (Interval {min, max}) = min == PosInf || max == NegInf

{-| Convert an interval to a list of restrictions. -}
intervalToOrders : Interval a -> List (Order, a)
intervalToOrders (Interval { min, max }) =
  case (min, max) of
    (NegInf, PosInf) -> []
    (NegInf, Fin b)  -> [(LT, b)]
    (Fin a,  PosInf) -> [(GT, a)]
    (Fin a,  Fin b)  -> [(GT, a), (LT, b)]
    -- (PosInf, _)      -> [(LT, a),(GT, a)]
    -- (_,      NegInf) -> [(LT, a),(GT, a)]

{-| Map over the endpoints of the interval. -}
map : (a -> b) -> Interval a -> Interval b
map f (Interval a) = Interval { min = mapEndpoint f a.min, max = mapEndpoint f a.max }

{-| The intersection of two intervals. If the intervals overlap, this is the common part. If not, this is the empty interval. -}
intersection : Interval comparable -> Interval comparable -> Interval comparable
intersection (Interval a) (Interval b) = Interval { min = maxEndPoint a.min b.min, max = minEndPoint a.max b.max }

{-| The convex hull of two intervals. This is similar to union in that it includes all the points of the component intervals,
    and for non-overlapping intervals, the points inbetween them. -}
hull : Interval comparable -> Interval comparable -> Interval comparable
hull (Interval a) (Interval b)= Interval { min = minEndPoint a.min b.min, max = maxEndPoint a.max b.max }

{-| Convert a restriction to an interval. -}
orderToInterval : Order -> a -> Interval a
orderToInterval o x = case o of
  LT -> Interval { min = NegInf, max = Fin x }
  GT -> Interval { min = Fin x,  max = PosInf }
  -- TODO what to do for EQ (can't represent with open interval)

{-| Convert a list of restrictions to an interval. -}
ordersToIntervals : List (Order, comparable) -> Interval comparable
ordersToIntervals = List.foldr intersection full << List.map (Basics.uncurry orderToInterval)
