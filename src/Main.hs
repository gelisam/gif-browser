module Main where

import Codec.Picture.Gif
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Environment
import qualified Data.ByteString as ByteString


pinkBg :: Color
pinkBg = makeColorI 0xfe 0xe5 0xfc 0xff

bg :: Picture
bg = color pinkBg
   $ rectangleSolid 640 339

app :: [Picture] -> IO ()
app frames = do
  display
    (InWindow "Nice Window" (678, 427) (10, 10))
    white
    (bg <> frame 0)
  where
    frame :: Int -> Picture
    frame i = frames !! i

main :: IO ()
main = do
  args <- getArgs
  case args of
    [gifPath] -> do
      gifBytestring <- ByteString.readFile gifPath
      case decodeGifImages gifBytestring of
        Left err -> do
          error err
        Right frameDynamicImages -> do
          case traverse fromDynamicImage frameDynamicImages of
            Nothing -> do
              error "gloss does not support this image format"
            Just framePictures -> do
              app framePictures
    _ -> do
      putStrLn "usage: gif-browser GIF_FILE"
