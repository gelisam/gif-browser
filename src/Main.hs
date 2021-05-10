{-# LANGUAGE MultiWayIf #-}
module Main where

import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Interact
import System.Environment
import qualified Data.ByteString as ByteString


pinkBg :: Color
pinkBg = makeColorI 0xfe 0xe5 0xfc 0xff

bgWidth :: Num a => a
bgWidth = 426

bgHeight :: Num a => a
bgHeight = 230

backgroundPicture :: Picture
backgroundPicture
  = color pinkBg
  $ rectangleSolid bgWidth bgHeight

app :: [Picture] -> IO ()
app frames = do
  play
    (InWindow
      "gif-browser"
      (bgWidth + 2*20, bgHeight + 2*45)
      (10, 10))
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
      = scale 1 1 (frames !! i)

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

removePixelBg
  :: Image PixelRGBA8
  -> PixelRGBA8
  -> Int
  -> Int -> Int -> PixelRGBA8
  -> PixelRGBA8
removePixelBg img bg threshold x y _
  = if flip all [-2..2] $ \dx
    -> flip all [-2..2] $ \dy
    -> isBgColor (pixelAtDelta dx dy)
    then PixelRGBA8 0x00 0x00 0x00 0x00
    else pixelAtDelta 0 0
  where
    isInBounds :: Int -> Int -> Bool
    isInBounds i j
      = i >= 0
     && j >= 0
     && i < imageWidth img
     && j < imageHeight img

    pixelOrBg :: Int -> Int -> PixelRGBA8
    pixelOrBg i j
      | isInBounds i j
        = pixelAt img i j
      | otherwise
        = bg

    pixelAtDelta :: Int -> Int -> PixelRGBA8
    pixelAtDelta dx dy = pixelOrBg (x + dx) (y + dy)

    isBgColor :: PixelRGBA8 -> Bool
    isBgColor c = colorDistance c bg < threshold

removeImageBg
  :: PixelRGBA8
  -> Int
  -> Image PixelRGBA8
  -> Image PixelRGBA8
removeImageBg bg threshold img
  = pixelMapXY (removePixelBg img bg threshold) img

convertPicture :: DynamicImage -> Picture
convertPicture
  = fromImageRGBA8
  . removeImageBg (PixelRGBA8 0xff 0xff 0xff 0xff) 92
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
