module Main where

import Linear (V2(..), V4(..))
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, allocateRenderResource, RenderResource)
import Sample

main :: IO ()
main = do
    let width  = 640
        height = 480

        lineColor = V4 0 0 0 1
        fillColor = V4 0 1 0 1

        lineStyle = LineStyle lineColor 0.8
        fillStyle = PlainColorFillStyle fillColor
        style = (ShapeStyle (Just lineStyle) fillStyle)

        divCount = 4
        dx = fromIntegral width / fromIntegral divCount
        dy = fromIntegral height / fromIntegral divCount
        radius = dx * 0.2

        drawings = do
            i <- [0..(divCount - 1)]
            j <- [0..(divCount - 1)]
            let x0 = fromIntegral i * dx
                y0 = fromIntegral j * dy
                r = 1
                circle = Circle (V2 x0 y0) radius
                style = ShapeStyle (Just lineStyle) (PlainColorFillStyle $ V4 r 0 0 1)
            return $ ShapeDrawing style circle

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) drawings
        init = do
            resource <- allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"
