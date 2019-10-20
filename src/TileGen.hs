module TileGen where
    import           Codec.Picture
    import           Codec.Picture.Extra

    import           Data.List as L
    import           Data.Ord

    import qualified Data.Map as Map
    import           Data.Map (Map)
    import           Data.Tree

    import           Debug.Trace
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
    type Neighbor = (CoOrd,CoOrd)

    data ValidPair = ValidPair
        { tileA :: Int
        , tileB :: Int
        , dir   :: CoOrd
        }
    instance Eq ValidPair where
        v1 == v2 = and [(tileA v1 == tileA v2), (tileB v1 == tileB v2), (dir v1 == dir v2)]
    instance Show ValidPair where 
        show (ValidPair a b d) = "{" ++ show a ++ ", " ++ show b ++ ", " ++ show d ++ "}" 

    defaultTilePixel = PixelRGB8 255 0 127

    dirs = [(0,-1),(-1,0),(0,1),(1,0)]

    emptyWave = Map.empty

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
        [ValidPair a b d | a <- [0.. length ts - 1], b <- [0.. length ts - 1], d <- dirs]

    compareWithOffset :: TileImg -> TileImg -> CoOrd -> Bool
    compareWithOffset a b (x,y) =
        let comA = getOffset a (x,y)
            comB = getOffset b (-x,-y)
        in  comA == comB

    getOffset :: TileImg -> CoOrd -> TileImg
    getOffset t (0,-1) = crop 0 0 3 2 t
    getOffset t (-1,0) = crop 0 0 2 3 t
    getOffset t (0,1)  = crop 0 1 3 2 t
    getOffset t (1,0)  = crop 1 0 2 3 t

    --todo: fix this shit. causing index out of bounds error
    
    --take in the input, return the frequency of unique tiles
    getTileFrequencies :: [TileImg] -> [TileImg] -> TileFreqs
    getTileFrequencies pattern ts = Map.toList $ foldl
        (\m t -> Map.insertWith (+) (fromJust $ elemIndex t ts) 1.0 m) Map.empty
        pattern

    --look through input and select all the unique tiles
    getUniqueTiles :: [TileImg] -> [TileImg] -> [TileImg]
    getUniqueTiles pattern ts = foldl
        (\l t -> addToSet l t) ts $
        pattern

    addToSet :: Eq a => [a] -> a -> [a]
    addToSet l e
        | elem e l  = l
        | otherwise = l ++ [e]

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
        [(c, sum (L.map snd fs)) | (c, fs) <- Map.toList w]
        
    indexPix :: TileImg -> Pix
    indexPix t = pixelAt t 0 0

    observePixel :: Wave -> StdGen -> CoOrd -> (Int, StdGen)
    observePixel w seed coord = Rand.runRand (Rand.fromList $ w Map.! coord) seed

    simplePropagation :: StdGen -> [ValidPair] -> Int -> [Neighbor] -> Wave ->  Either StdGen Wave  
    simplePropagation _ _ _ [] w = Right w
    simplePropagation s pairs newTile ((d,n):ns) w =
        case collapsePixel pairs [newTile] d (w Map.! n) of
            [] -> Left s
            nPoss -> simplePropagation s pairs newTile ns (Map.insert n nPoss w)

    getNeighbors :: Wave -> CoOrd -> [Neighbor]
    getNeighbors w (x,y) = 
        filter (\(d,n) -> elem n (Map.keys w)) [((dx,dy),(x+dx,y+dy)) | (dx,dy) <- dirs]

    collapsePixel :: [ValidPair] -> [Int] -> CoOrd -> [(Int, Rational)] -> [(Int, Rational)]
    collapsePixel pairs enablers dir possible = foldl
        (\ps e -> collapsePixel' pairs e dir ps) 
        possible
        enablers
    
    collapsePixel' pairs enabler dir possible =
        filter (\(p,r) -> elem (ValidPair enabler p dir) pairs) possible

    --collapseWave :: [ValidPair] -> Wave -> Collapsed -> StdGen -> Either StdGen Collapsed
    collapseWave pairs w cw seed  
        | Map.keys w == [] = Right cw
        | otherwise = do
            let nCoOrd         = selectNextCoOrd w
                (nTile, nSeed) = observePixel w seed nCoOrd
                nCw            = Map.insert nCoOrd nTile cw
            nW <- simplePropagation nSeed pairs nTile (getNeighbors w nCoOrd) (Map.delete nCoOrd w)
            collapseWave pairs nW nCw nSeed

--Functions for outputting generated images
--------------------------------------------------------------------------------------------------------

    generatePixelList :: Collapsed -> [TileImg] -> Map CoOrd Pix
    generatePixelList cw ts =
        Map.fromList [(c, indexPix $ ts !! i) | (c,i) <- Map.toList cw]


    generateOutputImage :: Map CoOrd Pix -> Int -> Int -> TileImg
    generateOutputImage ps x y =
        generateImage (\x y -> ps Map.! (x,y)) x y

    generateUntilValid pairs w m s =
        case collapseWave pairs w m s of
            Left ns -> trace ("contradiction") $ generateUntilValid pairs w m ns
            Right cw -> cw

    testout = do
        Right input <- readPng "tall-grid-input.png"
        let conv = convertRGB8 input
            (tiles, freqs) = getTileData conv 3
            pairs = getAdjacencyRules tiles
            w = generateStartingWave (200,200) freqs
            cw = generateUntilValid pairs w Map.empty (mkStdGen 2)
        let pxs = generatePixelList cw tiles
            out = generateOutputImage pxs 200 200
        writePng "testout.png" out


    --TODO: write function that selects next tile, collapse wave upon observation