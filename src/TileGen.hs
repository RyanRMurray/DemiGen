module TileGen where
    import           Codec.Picture
    import           Codec.Picture.Extra

    import           Data.List as L
    import           Data.Ord

    import qualified Data.Map as M
    import           Data.Map (Map)
    import qualified Data.Heap as H
    import           Data.Heap (MinHeap)
    import qualified Data.Set as S
    import           Data.Set (Set)

    import           Debug.Trace
    import           Data.Maybe

    import           System.Random
    import           Control.Monad.Random as Rand

    type Pix = PixelRGB8 
    type TileImg = Image Pix              -- Type of image to import and export
    type Tiles = Map Int TileImg
    type TileFreqs = Set (Int, Rational)
    type CoOrd = (Int, Int)
    type Wave = Map CoOrd TileFreqs
    type Collapsed = Map CoOrd Int
    type Neighbor = (CoOrd,CoOrd)
    type EntropyHeap = MinHeap (Rational, CoOrd)
    type EnabledTiles = Map (Int, CoOrd) (Set Int)

    defaultTilePixel = PixelRGB8 255 0 127

    dirs = [(0,-1),(-1,0),(0,1),(1,0)]

    emptyWave = M.empty

--Functions for parsing an input image
--------------------------------------------------------------------------------------------------------

    getTileData :: TileImg -> Int -> ([TileImg], TileFreqs)
    getTileData pattern n =
        let input       = getTiles pattern n
            uniqueTiles = getUniqueTiles input []
            tFreq       = getTileFrequencies input uniqueTiles
        in
            (uniqueTiles, tFreq)

    getTilesWithRotation :: TileImg -> Int -> ([TileImg], TileFreqs)
    getTilesWithRotation pattern n =
        let input = concat [getTiles pattern n, getTiles (rotateLeft90 pattern) n, getTiles (rotate180 pattern) n, getTiles (rotateRight90 pattern) n]
            uniqueTiles = getUniqueTiles input []
            tFreq = getTileFrequencies input uniqueTiles
        in
            (uniqueTiles, tFreq)

    --take a set of (unique) tiles and 
    getAdjacencyRules :: [TileImg] -> [(Int, Int, CoOrd)]
    getAdjacencyRules ts = L.filter 
        (\(a,b,d) -> compareWithOffset (ts !! a) (ts !! b) d) 
        [(a, b, d) | a <- [0.. length ts - 1], b <- [0.. length ts - 1], d <- dirs]

    processAdjacencyRules :: [TileImg] -> EnabledTiles
    processAdjacencyRules ts = foldl 
            (\m (a,b,d)-> M.insertWith (S.union) (a,d) (S.singleton b) m)
            M.empty $
            getAdjacencyRules ts


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
    
    --take in the input, return the frequency of unique tiles
    getTileFrequencies :: [TileImg] -> [TileImg] -> TileFreqs
    getTileFrequencies pattern ts = 
        let counts = getTileFrequencies' pattern ts
            total    = sum $ S.map snd counts
        in
            S.map (\(i,w)-> (i, w/total)) counts

    getTileFrequencies' pattern ts = S.fromList $ M.toList $ foldl (\m t -> M.insertWith (+) (fromJust $ elemIndex t ts) 1.0 m) M.empty pattern

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
        (x,y) <- (getGrid (imageWidth pattern -1) (imageHeight pattern -1))]

    repeatPattern :: TileImg -> TileImg
    repeatPattern p = below [beside [p, p], beside [p, p]]

--Functions for collapsing a wave
--------------------------------------------------------------------------------------------------------

    generateStartingWave :: CoOrd -> TileFreqs -> Wave
    generateStartingWave (x,y) freqs = foldl
        (\m pos -> M.insert pos freqs m) M.empty $
        getGrid x y

    selectNextCoOrd :: Wave -> EntropyHeap -> (CoOrd, EntropyHeap)
    selectNextCoOrd w h 
            | M.member c w = (c,nH)
            | otherwise    = selectNextCoOrd w nH
           where c  = snd $ head $ H.take 1 h
                 nH = H.drop 1 h

    indexPix :: TileImg -> Pix
    indexPix t = pixelAt t 0 0

    observePixel :: Wave -> StdGen -> CoOrd -> (Int, StdGen)
    observePixel w seed coord = Rand.runRand (Rand.fromList $ S.toList $ w M.! coord) seed


    propagate :: StdGen -> EnabledTiles -> [CoOrd] -> Wave -> EntropyHeap -> Either StdGen (Wave, EntropyHeap)
    propagate _ _ [] w h = Right (w,h)
    propagate s rules (t:ts) w h =
        case collapseNeighbors s rules (S.map fst $ w M.! t) (getNeighbors w t) w h [] of
            Left err -> Left err
            Right (nW, nH, next) -> propagate s rules (next ++ ts) nW nH


    
    collapseNeighbors :: StdGen -> EnabledTiles -> Set Int -> [Neighbor] -> Wave -> EntropyHeap -> [CoOrd] -> Either StdGen (Wave, EntropyHeap, [CoOrd])
    collapseNeighbors _ _ _ [] w h next = Right (w,h,next)
    collapseNeighbors s rules enablers ((d,n):ns) w h next 
        | length collapsedPixel == length precollapsed = collapseNeighbors s rules enablers ns w h next
        | length collapsedPixel == 0 = Left s
        | otherwise                                    = collapseNeighbors s rules enablers ns newWave newHeap $ next ++ [n]
        where precollapsed   = w M.! n
              collapsedPixel = collapsePixel rules enablers d precollapsed
              newWave        = M.insert n collapsedPixel w
              newHeap        = H.insert (getEntropy collapsedPixel, n) h

    getEntropy :: Set (Int, Rational) -> Rational
    getEntropy weights = toRational $ - sum [realToFrac w * logBase 2 (realToFrac w :: Float)| (_, w) <- S.toList weights]

    getNeighbors :: Wave -> CoOrd -> [Neighbor]
    getNeighbors w (x,y) = 
        L.filter (\(d,n) -> M.member n w) [((dx,dy),(x+dx,y+dy)) | (dx,dy) <- dirs]

    collapsePixel :: EnabledTiles -> Set Int -> CoOrd -> Set (Int, Rational) -> Set (Int, Rational)
    collapsePixel rules enablers dir possible = 
        S.filter (\(i, _)-> S.member i allowed) possible 
        where
            allowed = S.unions [collapsePixel' rules e dir | e <- S.toList enablers]
    
    collapsePixel' rules enabler dir  =
        M.findWithDefault S.empty (enabler, dir) rules

    collapseWave :: EnabledTiles -> Wave -> Collapsed -> EntropyHeap -> StdGen -> Either StdGen Collapsed
    collapseWave rules w cw h seed  
        | M.keys w == [] = Right cw
        | otherwise = do
            let (nCoOrd, nH)   = selectNextCoOrd w h
                (nTile, nSeed) = observePixel w seed nCoOrd
                nCw            = M.insert nCoOrd nTile cw
            (nW, nH2) <- propagate nSeed rules [nCoOrd] (M.insert nCoOrd (S.singleton (nTile,1.0)) w) nH
            collapseWave rules (M.delete nCoOrd nW) nCw nH2 nSeed

--Functions for outputting generated images
--------------------------------------------------------------------------------------------------------

    generatePixelList :: Collapsed -> [TileImg] -> Map CoOrd Pix
    generatePixelList cw ts =
        M.fromList [(c, indexPix $ ts !! i) | (c,i) <- M.toList cw]


    generateOutputImage :: Map CoOrd Pix -> Int -> Int -> TileImg
    generateOutputImage ps x y =
        generateImage (\x y -> ps M.! (x,y)) x y

    generateUntilValid pairs w m h s =
        case collapseWave pairs w m h s of
            Left ns -> trace ("contradiction") $ generateUntilValid pairs w m h ns
            Right cw -> cw

    testout = do
        Right input <- readPng "Dungeon.png"
        let conv = convertRGB8 input
            (tiles, freqs) = getTilesWithRotation conv 3
            rules = processAdjacencyRules tiles
            w = generateStartingWave (160,160) freqs
            h = H.singleton (0.0, (3,3)) :: EntropyHeap
            cw = generateUntilValid rules w M.empty h (mkStdGen 420)
        let pxs = generatePixelList cw tiles
            out = generateOutputImage pxs 160 160
        writePng "testout.png" out