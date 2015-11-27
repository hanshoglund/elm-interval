
module Interval.EndPoint (
    EndPoint(..),
    maxEndPoint,
    minEndPoint,
    mapEndpoint
  ) where
{-| Defines endpoints for intervals.

    This is an [extended real number line](https://en.wikipedia.org/wiki/Extended_real_number_line).

@docs EndPoint
@docs maxEndPoint, minEndPoint, mapEndpoint
-}

{-| An endpoint, representing positive or negative infinity, or a finite value. -}
type EndPoint a = NegInf | Fin a | PosInf

{-| The maximum of two endpoints. -}
maxEndPoint : EndPoint comparable -> EndPoint comparable -> EndPoint comparable
maxEndPoint x y = case (x, y) of
  (PosInf,   _)     -> PosInf
  (_,     PosInf)   -> PosInf
  (NegInf,   y)     -> y
  (x,     NegInf)   -> x
  (Fin x, Fin y)    -> Fin (x `max` y)

{-| The minimum of two endpoints. -}
minEndPoint : EndPoint comparable -> EndPoint comparable -> EndPoint comparable
minEndPoint x y = case (x, y) of
  (NegInf,   _)     -> NegInf
  (_,     NegInf)   -> NegInf
  (PosInf,   y)     -> y
  (x,     PosInf)   -> x
  (Fin x, Fin y)    -> Fin (x `min` y)

{-| Map over an endpoint. -}
mapEndpoint : (a -> b) -> EndPoint a -> EndPoint b
mapEndpoint f x = case x of
  Fin x  -> Fin (f x)
  NegInf -> NegInf
  PosInf -> PosInf
