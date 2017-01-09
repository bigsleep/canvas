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

import Data.Text (Text)
import Linear (V2, V3, V4(..), M22)

type Coord = V2 Float

type Color = V4 Float

data Shape
    = Triangle !Coord !Coord !Coord
    | Rectangle !Coord !Float !Float
    | Circle !Coord !Float
    | RoundRect !Coord !Float !Float !Float
    deriving (Show, Read, Eq)

data Path
    = StripPath ![Coord]
-- | LoopPath ![Coord]
-- | FragmentPath ![Coord]
    | Arc !Coord !Float !Float !Float
    deriving (Show, Read, Eq)

data ShapeStyle = ShapeStyle
    { shapeStyleLineStyle :: !(Maybe LineStyle)
    , shapeStyleFillStyle :: !FillStyle
    }
    deriving (Show, Read, Eq)

data LineStyle = LineStyle
    { lineStyleColor :: !Color
    , lineStyleWidth :: !Float
    }
    deriving (Show, Read, Eq)

data FillStyle
    = PlainColorFillStyle !Color
    | TexturedFillStyle !TextureRange
    deriving (Show, Read, Eq)

data Transform
    = Rotate !Float !Coord
    | Translate !Coord
    | Scale !Float
    | Affine !(M22 Float)
    deriving (Show, Read, Eq)

type Transforms = [Transform]

data Drawing
    = ShapeDrawing !ShapeStyle !Shape
    | PathDrawing !LineStyle !Path
    deriving (Show, Read, Eq)

data Canvas
    = Canvas
    { canvasOrigin :: !Coord
    , canvasWidth :: !Float
    , canvasHeight :: !Float
    , canvasDrawings :: ![Drawing]
    } deriving (Show, Read, Eq)

data TextureRange = TextureRange
    { trTextureName :: !Text
    , trTextureLeftUp :: !Coord
    , trTextureRightDown :: !Coord
    } deriving (Show, Read, Eq)
