module TileGen where
    import           Codec.Picture
    import           Codec.Picture.Extra

    import           Data.List as L
    import           Data.Ord

    import qualified Data.Map as Map
    import           Data.Map (Map)

    import           Data.Maybe

    import           System.Random
    import           Control.Monad.Random as Rand

    type Pix = PixelRGB8 
    type TileImg = Image Pix              -- Type of image to import and export
    type Tiles = Map Int TileImg
    type TileFreqs = Map Int Rational
    type CoOrd = (Int, Int)
    type Wave = Map CoOrd TileFreqs
    type Collapsed = Map CoOrd Int
    

    data ValidPair = ValidPair
        { tileA :: Int
        , tileB :: Int
        , dir   :: CoOrd
        }

    defaultTilePixel = PixelRGB8 255 0 127

    dirs = [(0,-1),(-1,0),(0,1),(1,0)]

--Functions for parsing an input image
--------------------------------------------------------------------------------------------------------

    getTileData :: TileImg -> Int -> ([TileImg], TileFreqs)
    getTileData pattern n =
        let input       = getTiles pattern n
            uniqueTiles = getUniqueTiles input []
            tFreq       = getTileFrequencies input uniqueTiles
        in
            (uniqueTiles, tFreq)

    --take a set of (unique) tiles and 
    getAdjacencyRules :: [TileImg] -> [ValidPair]
    getAdjacencyRules ts = filter 
        (\v -> compareWithOffset (ts !! tileA v) (ts !! tileB v) (dir v)) 
        [ValidPair a b d | a <- [0.. length ts], b <- [0.. length ts], d <- dirs]

    compareWithOffset :: TileImg -> TileImg -> CoOrd -> Bool
    compareWithOffset a b (x,y) =
        let comA = getOffset a (x,y)
            comB = getOffset b (-x,-y)
        in  comA == comB

    getOffset :: TileImg -> CoOrd -> TileImg
    getOffset t (x,y) = crop (0+x) (0+y) (imageWidth t + x) (imageHeight t + x) t
    
    --take in the input, return the frequency of unique tiles
    getTileFrequencies :: [TileImg] -> [TileImg] -> TileFreqs
    getTileFrequencies pattern ts = foldl
        (\m t -> Map.insertWith (+) (fromJust $ elemIndex t ts) 1.0 m) Map.empty
        pattern

    --look through input and select all the unique tiles
    getUniqueTiles :: [TileImg] -> [TileImg] -> [TileImg]
    getUniqueTiles pattern ts = foldl
        (\l t -> getUniqueTiles' l t) ts $
        pattern
    
    getUniqueTiles' l t
        | elem t l  = l
        | otherwise = l ++ [t]

    getGrid :: Int -> Int -> [(Int, Int)]
    getGrid x y = concat [getGrid' x row| row <- [0..y]]
    getGrid' x row = [(col,row) | col <- [0..x]]

    getTiles :: TileImg -> Int -> [TileImg]
    getTiles pattern n = 
        [crop x y n n (repeatPattern pattern) | 
        (x,y) <- (getGrid (imageWidth pattern - 1) (imageHeight pattern - 1))]

    repeatPattern :: TileImg -> TileImg
    repeatPattern p = below [beside [p, p], beside [p, p]]

--Functions for collapsing a wave
--------------------------------------------------------------------------------------------------------

    generateStartingWave :: CoOrd -> TileFreqs -> Wave
    generateStartingWave (x,y) freqs = foldl
        (\m pos -> Map.insert pos freqs m) Map.empty $
        getGrid x y

    selectNextCoOrd :: Wave -> CoOrd
    selectNextCoOrd w = fst $ minimumBy (comparing snd)
        [(c, sum (Map.elems fs)) | (c, fs) <- Map.toList w]
        
    indexPix :: TileImg -> Pix
    indexPix t = pixelAt t 0 0

    --TODO: write function that selects next tile, collapse wave upon observation