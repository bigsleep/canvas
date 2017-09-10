module Graphics.Canvas.Geometry
    ( Triangle(..)
    , Circle(..)
    , triangleSideLengths
    , triangleArea
    , triangleAreaHeron
    , triangleIncircle
    , rescaleCoords
    ) where

import Control.Monad.Zip (MonadZip(..))
import Linear (V2(..), V3(..), Additive(..), Epsilon(..), distance, (*!), (*^), (^/))

data Triangle a = Triangle (V2 a) (V2 a) (V2 a)
    deriving (Show, Eq)

data Circle a = Circle a (V2 a)
    deriving (Show, Eq)

triangleSideLengths :: (Floating a) => Triangle a -> V3 a
triangleSideLengths (Triangle p0 p1 p2) = fmap (uncurry distance) $ mzip ps ps'
    where
    ps = V3 p1 p2 p0
    ps' = V3 p2 p0 p1

triangleArea :: (Floating a) => Triangle a -> a
triangleArea = triangleAreaHeron . triangleSideLengths

triangleAreaHeron :: (Floating a) => V3 a -> a
triangleAreaHeron (V3 l0 l1 l2) = sqrt $ s * (s - l0) * (s - l1) * (s - l2)
    where
    s = 0.5 * (l0 + l1 + l2)

triangleIncircle :: (Floating a, Epsilon a) => Triangle a -> Circle a
triangleIncircle t @ (Triangle p0 p1 p2) = Circle radius center
    where
    ps = V3 p0 p1 p2
    ls = triangleSideLengths t
    V3 l0 l1 l2 = ls
    lsum = l0 + l1 + l2
    a = triangleAreaHeron ls
    radius = if nearZero lsum
        then 0
        else 2 * a / lsum
    center = if nearZero lsum
        then (V3 (1/3) (1/3) (1/3)) *! ps
        else ls *! ps ^/ lsum

rescaleCoords :: (Floating a) => [V2 a] -> a -> V2 a -> [V2 a]
rescaleCoords coords scale o = fmap f coords
    where
    o' = (1 - scale) *^ o
    f v = (scale *^ v) ^+^ o'
