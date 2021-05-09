module Main where

import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Environment
import qualified Data.ByteString as ByteString


pinkBg :: Color
pinkBg = makeColorI 0xfe 0xe5 0xfc 0xff

backgroundPicture :: Picture
backgroundPicture
  = color pinkBg
  $ rectangleSolid 640 339

app :: [Picture] -> IO ()
app frames = do
  display
    (InWindow "Nice Window" (678, 427) (10, 10))
    white
    (backgroundPicture <> frame 0)
  where
    frame :: Int -> Picture
    frame i = scale 1.5 1.5 (frames !! i)

colorDistance :: PixelRGBA8 -> PixelRGBA8 -> Int
colorDistance (PixelRGBA8 r1 g1 b1 a1)
              (PixelRGBA8 r2 g2 b2 a2)
  = abs (fromIntegral r1 - fromIntegral r2)
  + abs (fromIntegral g1 - fromIntegral g2)
  + abs (fromIntegral b1 - fromIntegral b2)
  + abs (fromIntegral a1 - fromIntegral a2)

removeBg :: PixelRGBA8 -> Int -> PixelRGBA8 -> PixelRGBA8
removeBg bg threshold c
  = if colorDistance c bg <= threshold
    then PixelRGBA8 0x00 0x00 0x00 0x00
    else c

convertPicture :: DynamicImage -> Picture
convertPicture
  = fromImageRGBA8
  . pixelMap (removeBg (PixelRGBA8 0xff 0xff 0xff 0xff) 32)
  . convertRGBA8

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
          let framePictures :: [Picture]
              framePictures = fmap convertPicture
                                   frameDynamicImages
          app framePictures
    _ -> do
      putStrLn "usage: gif-browser GIF_FILE"
