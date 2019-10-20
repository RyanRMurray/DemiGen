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
    type TileFreqs = [(Int, Rational)]
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
    
    getTileData :: TileImg -> Int -> (TileFreqs, [TileImg])
    getTileData pattern n = 
        let ts = getTileFreqs pattern n
        in  ([],[])

    getTileFreqs :: TileImg -> Int -> [(TileImg, Rational)]
    getTileFreqs pattern n = foldl
        (\fs t -> incrementTileFreq t fs) [] $
        getTiles pattern n

    getGrid :: Int -> Int -> [(Int, Int)]
    getGrid x y = concat [getGrid' x row| row <- [0..y]]
    getGrid' x row = [(col,row) | col <- [0..x]]

    incrementTileFreq :: TileImg -> [(TileImg, Rational)] -> [(TileImg, Rational)]
    incrementTileFreq t [] = [(t,1.0)]
    incrementTileFreq t (f:fs)
        | t == fst f = [(t, snd f + 1.0)] ++ fs
        | otherwise  = f : incrementTileFreq t fs

    getTiles :: TileImg -> Int -> [TileImg]
    getTiles pattern n = 
        [crop x y n n (repeatPattern pattern) | 
        (x,y) <- (getGrid (imageWidth pattern - 1) (imageHeight pattern - 1))]

    repeatPattern :: TileImg -> TileImg
    repeatPattern p = below [beside [p, p], beside [p, p]]

    testout = do
        Right input <- readPng "tall-grid-input.png"
        let conv = convertRGB8 input
            tiles = beside $ map fst $ getTileFreq conv 3
        writePng "testout.png" tiles


--Functions for collapsing a wave
--------------------------------------------------------------------------------------------------------

    generateStartingWave :: CoOrd -> TileFreqs -> Wave
    generateStartingWave (x,y) freqs = foldl
        (\m pos -> Map.insert pos freqs m) Map.empty $
        getGrid x y

    selectNextCoOrd :: Wave -> CoOrd
    selectNextCoOrd w = fst $ minimumBy (comparing snd)
        [(c, sum (L.map snd fs)) | (c, fs) <- Map.toList w]
        
    indexPix :: TileImg -> Pix
    indexPix t = pixelAt t 0 0

