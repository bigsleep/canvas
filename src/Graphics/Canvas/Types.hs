module Graphics.Canvas.Types
    (
    ) where

import qualified Data.Color (Color)
import qualified Linear (V2(..))

type Coord = V2 Double

type Color = Data.Color.Color

data Shape
    = Triangle !Coord !Coord !Coord
    | Rectangle !Coord !Double !Double
    | Polygon ![Coord]
    | Circle !Coord !Double
    deriving (Show, Read, Eq)

data Path
    = LoopPath ![Coord]
    | StripPath ![Coord]
    | FragmentPath ![Coord]
    | Arc !Coord !Double !Double !Double
    deriving (Show, Read, Eq)

type ShapeStyle = ShapeStyle
    { lineStyle :: LineStyle
    , fillStyle :: FillStyle
    }
    deriving (Show, Read, Eq)

type LineStyle = LineStyle
    { color : Color
    , width : Double
    }
    deriving (Show, Read, Eq)

type FillStyle =
    { color :: Color
    }
    deriving (Show, Read, Eq)

type Drawing
    = ShapeDrawing !ShapeStyle !Transform !Shape
    | PathDrawing !LineStyle !Transform !Path
    deriving (Show, Read, Eq)

