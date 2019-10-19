module TileGen where
    import           Codec.Picture
    import           Codec.Picture.Extra

    import           Data.List as L
    import           Data.List.Split
    import           Data.Array
    import           Data.Ix (range)

    import qualified Data.Map as Map
    import           Data.Map (Map)

    import           Data.Maybe

    import           System.Random
    import           Control.Monad.Random as Rand

    type TileImg = Image PixelRGB8              -- Type of image to import and export
    type Pix = PixelRGB8 
    type TileFreqs = Map TileImg Rational

    data Pair = Pair
        { lTile :: TileImg
        , rTile :: TileImg
        }

    defaultTilePixel = PixelRGB8 255 0 127

--Functions for parsing an input image
--------------------------------------------------------------------------------------------------------
   
    getTileFreq :: Image Pix -> Int -> [(TileImg, Rational)]
    getTileFreq pattern n = foldl
        (\fs t -> incrementTileFreq t fs) [] $
        getTiles pattern n

    getGrid :: Int -> Int -> [(Int, Int)]
    getGrid x y = concat [getGrid' x row| row <- [0..y]]
    getGrid' x row = [(col,row) | col <- [0..x]]

    incrementTileFreq :: Image Pix -> [(TileImg, Rational)] -> [(TileImg, Rational)]
    incrementTileFreq t [] = [(t,1.0)]
    incrementTileFreq t (f:fs)
        | t == fst f = [(t, snd f + 1.0)] ++ fs
        | otherwise  = f : incrementTileFreq t fs

    getTiles :: Image Pix -> Int -> [TileImg]
    getTiles pattern n = 
        [crop x y n n (repeatPattern pattern) | 
        (x,y) <- (getGrid (imageWidth pattern - 1) (imageHeight pattern - 1))]

    repeatPattern :: Image Pix -> Image Pix
    repeatPattern p = below [beside [p, p], beside [p, p]]

    testout = do
        Right input <- readPng "tall-grid-input.png"
        let conv = convertRGB8 input
            tiles = beside $ map fst $ getTileFreq conv 3
        writePng "testout.png" tiles

