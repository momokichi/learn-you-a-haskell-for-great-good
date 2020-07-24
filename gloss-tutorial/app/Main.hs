module Main where

import           Graphics.Gloss

window :: Display
window = InWindow "Hello, World!" (640, 480) (100,100)

main :: IO ()
-- main = display window white
  -- (translate (-150) (-10) . scale 0.5 0.5 $ text "Hello, World!")
-- main = display window cyan blank
-- main = animate window white (\t -> circle (20 * t))
-- main = animate window white (\t -> circle (20 * t) <> rectangleWire 50 50)
-- main = animate window white (\t -> translate (-200) (-10) . rotate (270*t) . scale (0.2 * t) (0.2 * t) $ text "Hello, Haskell")
main = animate window white (\t -> translate (-200) (-10) . scale (0.2 * t) (0.2 * t) $ text "Hello, Haskell")
