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

import qualified Data.Color (Color, RGBA(..))
import Data.Word (Word8)
import Linear (V2, V4(..), M22)

type Coord = V2 Float

type Color = Data.Color.Color Float

data Shape
    = Triangle !Coord !Coord !Coord
    | Rectangle !Coord !Float !Float
    | Circle !Coord !Float
    | RoundRect !Coord !Float !Float !Float
    deriving (Show, Read, Eq)

data Path
    = LoopPath ![Coord]
    | StripPath ![Coord]
    | FragmentPath ![Coord]
    | Arc !Coord !Float !Float !Float
    deriving (Show, Read, Eq)

data ShapeStyle = ShapeStyle
    { shapeStyleLineStyle :: !LineStyle
    , shapeStyleFillStyle :: !FillStyle
    }
    deriving (Show, Read, Eq)

data LineStyle = LineStyle
    { lineStyleColor :: !Color
    , lineStyleWidth :: !Float
    }
    deriving (Show, Read, Eq)

data FillStyle = FillStyle
    { fillStyleColor :: !Color
    }
    deriving (Show, Read, Eq)

data Transform
    = Rotate !Float !Coord
    | Translate !Coord
    | Scale !Float
    | Affine !(M22 Float)
    deriving (Show, Read, Eq)

type Transforms = [Transform]

data Drawing
    = ShapeDrawing !ShapeStyle !Transforms !Shape
    | PathDrawing !LineStyle !Transforms !Path
    deriving (Show, Read, Eq)

data Canvas
    = Canvas
    { canvasOrigin :: !Coord
    , canvasWidth :: !Float
    , canvasHeight :: !Float
    , canvasDrawings :: ![Drawing]
    } deriving (Show, Read, Eq)
