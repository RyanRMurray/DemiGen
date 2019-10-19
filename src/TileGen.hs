module TileGen where
    import           Graphics.Image

    import           Data.List as L
    import           Data.List.Split
    import           Data.Ord
    import           Data.Ix (range)

    import qualified Data.Map as Map
    import           Data.Map (Map)

    import           Data.Maybe

    import           System.Random
    import           Control.Monad.Random as Rand

    type TileImg = Image VS RGB Word8                 -- Type of image to import and export
    type Pix = Pixel RGB Word8 


    data Pair = Pair
        { lTile :: TileImg
        , rTile :: TileImg
        }

    defaultTilePixel = PixelRGB 255 0 127

    getInputData :: TileImg -> (Map Pix Rational, [TileImg], [Pair])
    getInputData pattern = 
        let
            freqs = getPixelFreq pattern (dims pattern)
        in
            (Map.empty, [], [])
    
    getPixelFreq :: TileImg -> (Int, Int) -> Map Pix Rational -> Map Pix Rational
    getPixelFreq pattern xy freqs = foldl 
        (\pos -> Map.insertWith (+) (index pattern pos) (1::Rational) freqs) $
        freqs $
        getGrid xy 
    
    incrementPixelFreq :: Map Pix Rational -> Pix -> Map Pix Rational
    incrementPixelFreq m p = Map.insertWith (+) p (1::Rational) m

    getGrid :: (Int, Int) -> [(Int, Int)]
    getGrid (x,y) = concat [getGrid' x row | row <- [0..y]]
    getGrid' x row = [(col,row)| col <- [0..x]]