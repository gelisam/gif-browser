{-# LANGUAGE MultiWayIf #-}
module Main where

import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Interact
import System.Environment
import qualified Data.ByteString as ByteString


pinkBg :: Color
pinkBg = makeColorI 0xfe 0xe5 0xfc 0xff

bgWidth :: Float
bgWidth = 640

bgHeight :: Float
bgHeight = 339

backgroundPicture :: Picture
backgroundPicture
  = color pinkBg
  $ rectangleSolid bgWidth bgHeight

app :: [Picture] -> IO ()
app frames = do
  play
    (InWindow "Nice Window" (678, 427) (10, 10))
    white
    0
    minIndex
    draw
    reactEvent
    reactTime
  where
    minIndex :: Int
    minIndex = 0

    maxIndex :: Int
    maxIndex = length frames - 1

    frame :: Int -> Picture
    frame i
      = scale 1.5 1.5 (frames !! i)

    leftBorder :: Picture
    leftBorder
      = translate (-bgWidth/2 - 5) 0
          (rectangleSolid 3 (bgHeight + 9))
     <> translate (-bgWidth/2 - 2) (bgHeight/2 + 5)
          (rectangleSolid 10 3)
     <> translate (-bgWidth/2 - 2) (-bgHeight/2 - 5)
          (rectangleSolid 10 3)

    rightBorder :: Picture
    rightBorder
      = translate (bgWidth/2 + 6) 0
          (rectangleSolid 3 (bgHeight + 9))
     <> translate (bgWidth/2 + 2) (bgHeight/2 + 5)
          (rectangleSolid 10 3)
     <> translate (bgWidth/2 + 2) (-bgHeight/2 - 5)
          (rectangleSolid 10 3)

    draw :: Int -> Picture
    draw i
      = backgroundPicture
     <> frame i
     <> if | i == minIndex -> leftBorder
           | i == maxIndex -> rightBorder
           | otherwise     -> mempty

    reactEvent :: Event -> Int -> Int
    reactEvent (EventKey (SpecialKey KeyHome) Down _ _) _
      = minIndex
    reactEvent (EventKey (SpecialKey KeyLeft) Down _ _) i
      | i > minIndex
      = i - 1
    reactEvent (EventKey (SpecialKey KeyRight) Down _ _) i
      | i < maxIndex
      = i + 1
    reactEvent (EventKey (SpecialKey KeyEnd) Down _ _) _
      = maxIndex
    reactEvent _ i
      = i

    reactTime :: Float -> Int -> Int
    reactTime _ i
      = i

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
      putStrLn "usage:"
      putStrLn "  gif-browser GIF_FILE"
      putStrLn ""
      putStrLn "Left/Right to navigate between frames,"
      putStrLn "Home/End to jump to the beginnin/end."
