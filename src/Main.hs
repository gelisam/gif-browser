module Main where

import Graphics.Gloss
import System.Environment


pinkBg :: Color
pinkBg = makeColorI 0xfe 0xe5 0xfc 0xff

main :: IO ()
main = do
  args <- getArgs
  case args of
    [gifPath] -> do
      display
        (InWindow "Nice Window" (678, 427) (10, 10))
        white
        ( color pinkBg
        $ rectangleSolid 640 339
        )
    _ -> do
      putStrLn "usage: gif-browser GIF_FILE"
