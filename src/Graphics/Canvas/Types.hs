module Graphics.Canvas.Types
    ( Coord
    , Color
    , Shape(..)
    , Path(..)
    , ShapeStyle(..)
    , LineStyle(..)
    , FillStyle(..)

    , Transforms
    , Drawing(..)
    , Canvas(..)
    ) where

import qualified Data.Color (Color)
import Data.Word (Word8)
import Linear (V2, M22)

type Coord = V2 Double

type Color = Data.Color.Color Word8

data Shape
    = Triangle !Coord !Coord !Coord
--    | Rectangle !Coord !Double !Double
--    | Polygon ![Coord]
--    | Circle !Coord !Double
    deriving (Show, Read, Eq)

data Path
    = LoopPath ![Coord]
    | StripPath ![Coord]
    | FragmentPath ![Coord]
    | Arc !Coord !Double !Double !Double
    deriving (Show, Read, Eq)

data ShapeStyle = ShapeStyle
    { shapeStyleLineStyle :: !LineStyle
    , shapeStyleFillStyle :: !FillStyle
    }
    deriving (Show, Read, Eq)

data LineStyle = LineStyle
    { lineStyleColor :: !Color
    , lineStyleWidth :: !Double
    }
    deriving (Show, Read, Eq)

data FillStyle = FillStyle
    { fillStyleColor :: !Color
    }
    deriving (Show, Read, Eq)

data Transform
    = Rotate !Double !Coord
    | Translate !Coord
    | Scale !Double
    | Affine !(M22 Double)
    deriving (Show, Read, Eq)

type Transforms = [Transform]

data Drawing
    = ShapeDrawing !ShapeStyle !Transforms !Shape
--    | PathDrawing !LineStyle !Transforms !Path
    deriving (Show, Read, Eq)

data Canvas
    = Canvas
    { canvasOrigin :: !Coord
    , canvasWidth :: !Double
    , canvasHeight :: !Double
    , canvasDrawings :: ![Drawing]
    } deriving (Show, Read, Eq)
