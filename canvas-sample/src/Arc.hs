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
        lineStyle = LineStyle lineColor 10
        startAngle = pi / 4
        endAngle = 3 * pi / 2

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
                arc = Arc (V2 x0 y0) radius startAngle endAngle
            return $ PathDrawing lineStyle arc

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) drawings
        init = do
            resource <- allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"
