module Main where

import Linear (V2(..), V4(..))
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, allocateRenderResource, RenderResource)
import Sample

main :: IO ()
main = do
    let width  = 640
        height = 480

        lineColor = V4 0 0 0 255
        lineStyle = LineStyle lineColor 4
        cornerRadius = 10

        divCount = 1
        dx = fromIntegral width / fromIntegral divCount
        dy = fromIntegral height / fromIntegral divCount

        drawings = do
            i <- [0..(divCount - 1)]
            j <- [0..(divCount - 1)]
            let x0 = fromIntegral i * dx
                y0 = fromIntegral j * dy
                rectangle = RoundRect (V2 x0 y0) (0.5 * dx) (0.5 * dy) cornerRadius
                style = ShapeStyle (Just lineStyle) (PlainColorFillStyle $ V4 255 0 0 255)
            return $ ShapeDrawing style rectangle

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) drawings
        init = do
            resource <- allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"
