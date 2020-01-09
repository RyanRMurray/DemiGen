module DemiGen.TileGen where
    import          DemiGen.Types

    import           Data.Maybe

    import           Data.List as L
    import qualified Data.Map as M
    import qualified Data.Heap as H
    import qualified Data.Set as S
    import           Data.Map (Map)
    import           Data.Heap (MinHeap)
    import           Data.Set (Set)
    import           Data.Ord
    import Debug.Trace

    import           System.Random.Mersenne.Pure64
    import           System.Random.Shuffle

    import           Codec.Picture
    import           Codec.Picture.Extra

--Functions for parsing an input image
--------------------------------------------------------------------------------------------------------

    --takes an input image, a tile size, and transformation options, returning a set of unique tiles and
    --their relative rarity.
    getTileData :: TileImg -> Int -> [Transform] -> ([TileImg], TileFreqs)
    getTileData pattern n [] = getTileData pattern n [noTransform]
    getTileData pattern n trans =
        let transmutations = [t pattern | t <- trans]
            tiles = concat $ L.map (getTiles n) transmutations
            unique = nub tiles
            tFreq = getTileFrequencies tiles unique
        in
            (unique, tFreq)


    --take in the input, return the frequency of unique tiles
    getTileFrequencies :: [TileImg] -> [TileImg] -> TileFreqs
    getTileFrequencies pattern ts =
        S.fromList 
        $ M.toList 
        $ foldl (\m t -> M.insertWith (+) (fromJust $ elemIndex t ts) 1 m) M.empty pattern

    --take a set of (unique) tiles and determine which can be overlapped as neighbors
    processAdjacencyRules :: Int -> [TileImg] -> AdjRules
    processAdjacencyRules n ts = foldl
            (\m (a,b,d)-> M.insertWith (S.union) (a,d) (S.singleton b) m)
            M.empty $
            getAdjacencyRules n ts

    getAdjacencyRules :: Int -> [TileImg] -> [(Int, Int, CoOrd)]
    getAdjacencyRules n ts = L.filter
        (\(a,b,d) -> compareWithOffset n (ts !! a) (ts !! b) d)
        [(a, b, d) | a <- [0.. length ts - 1], b <- [0.. length ts - 1], d <- dirs]

    compareWithOffset :: Int -> TileImg -> TileImg -> CoOrd -> Bool
    compareWithOffset n a b (x,y) =
        let comA = getOffset n a (x,y)
            comB = getOffset n b (-x,-y)
        in  comA == comB

    getOffset :: Int -> TileImg -> CoOrd -> TileImg
    getOffset n t (0,-1) = crop 0 0 n (n-1) t
    getOffset n t (-1,0) = crop 0 0 2 n t
    getOffset n t (0,1)  = crop 0 1 n (n-1) t
    getOffset n t (1,0)  = crop 1 0 (n-1) n t

    getTiles :: Int -> TileImg -> [TileImg]
    getTiles n pattern =
        [crop x y n n pattern |
        (x,y) <- (getGrid (imageWidth pattern -1) (imageHeight pattern -1))]

--Functions for collapsing a wave
--------------------------------------------------------------------------------------------------------

    --Core wave function collapse process. Loops until the Wave has been observed entirely, or a contradiction
    collapseWave :: AdjRules -> Wave -> Grid -> EntropyHeap -> PureMT -> Either PureMT Grid
    collapseWave rules w cw h seed
        | M.keys w == [] = Right cw
        | otherwise = do
            case selectNextCoOrd w h of
                Left () -> Right cw
                Right (nCoOrd, nH) -> do
                    (nTile, nSeed)    <- observePixel (w M.! nCoOrd) seed nCoOrd
                    let nCw           = M.insert nCoOrd nTile cw
                        (nW, nH2)     = propagate rules [nCoOrd] (M.insert nCoOrd (S.singleton (nTile,1)) w) nH
                    collapseWave rules (M.delete nCoOrd nW) nCw nH2 nSeed

    --select next coordinate, the one with lowest entropy
    selectNextCoOrd :: Wave -> EntropyHeap -> Either () (CoOrd, EntropyHeap) 
    selectNextCoOrd w h
            | H.size h == 0  = Left ()
            | M.member c w   = Right (c,nH)
            | otherwise      = selectNextCoOrd w nH
           where c  = snd $ head $ H.take 1 h
                 nH = H.drop 1 h

    --Collapse the probability space of a cell to a single tile
    observePixel :: TileFreqs -> PureMT -> CoOrd -> Either PureMT (Int, PureMT)
    observePixel fs seed coord
        | S.size fs == 0 = Left seed
        | otherwise      = Right $ randomFrom (S.toList fs) seed

    --Collapse probability space of connected cells based on adjacency rules. If a cell has no possible tiles,
    --return a contradiction.
    propagate :: AdjRules -> [CoOrd] -> Wave -> EntropyHeap -> (Wave, EntropyHeap)
    propagate _ [] w h = (w,h)
    propagate rules (t:ts) w h =
            propagate rules (nub $ ts ++ next) nW nH
        where
            (nW, nH, next) = collapseNeighbors rules (S.map fst $ w M.! t) (getNeighbors w t) w h []

    --Collapse the immediate neighbors of a tile, and if their probability space shrinks, collapse that cell's neighbors next
    collapseNeighbors :: AdjRules -> Set Int -> [Neighbor] -> Wave -> EntropyHeap -> [CoOrd] -> (Wave, EntropyHeap, [CoOrd])
    collapseNeighbors _ _ [] w h next = (w,h,next)
    collapseNeighbors rules enablers ((d,n):ns) w h next
        | newLength /= length precollapsed = collapseNeighbors rules enablers ns newWave newHeap $ next ++ [n]
        | otherwise                        = collapseNeighbors rules enablers ns w h next
        where precollapsed   = w M.! n
              collapsedPixel = collapsePixel rules enablers d precollapsed
              newWave        = M.insert n collapsedPixel w
              newHeap        = H.insert (getEntropy collapsedPixel, n) h
              newLength      = length collapsedPixel

    getEntropy :: Set (Int, Int) -> Double
    getEntropy weights =  sum [dw * logBase 2 dw | (_, w) <- S.toList weights, let dw = fromIntegral w]

    getNeighbors :: Wave -> CoOrd -> [Neighbor]
    getNeighbors w (x,y) =
        L.filter (\(d,n) -> M.member n w) [((dx,dy),(x+dx,y+dy)) | (dx,dy) <- dirs]

    collapsePixel :: AdjRules -> Set Int -> CoOrd -> Set (Int, Int) -> Set (Int, Int)
    collapsePixel rules enablers dir possible =
        S.filter (\(i, _)-> S.member i allowed) possible
        where
            allowed = S.unions [collapsePixel' rules enabler dir | enabler <- S.toList enablers]

    collapsePixel' rules enabler dir  =
        M.findWithDefault S.empty (enabler, dir) rules


--Functions for outputting generated images
--------------------------------------------------------------------------------------------------------

    indexPix :: TileImg -> Pix
    indexPix t = pixelAt t 0 0

    generatePixelList :: Grid -> [TileImg] -> Map CoOrd Pix
    generatePixelList cw ts =
        M.fromList [(c, indexPix $ ts !! i) | (c,i) <- M.toList cw]


    generateOutputImage :: Map CoOrd Pix -> TileImg
    generateOutputImage ps =
        generateImage 
            (\x y -> M.findWithDefault defaultTilePixel (x,y) ps) 
            (maxx + abs minx + 10) (maxy + abs miny + 10)
      where
        (minx,miny,maxx,maxy)  = getBounds (M.keys ps) (-5,-5,0,0)

--Top level functions for specifying tile generation parameters
--------------------------------------------------------------------------------------------------------

    generateStartingWave :: [CoOrd] -> TileFreqs -> Wave
    generateStartingWave coords freqs = foldl
        (\m pos -> M.insert pos freqs m) M.empty $
        coords

    --generate an image with these parameters. WARNING; NOT GUARUNTEED TO HALT.
    generateUntilValid :: AdjRules -> Wave -> Grid -> EntropyHeap -> PureMT -> Grid
    generateUntilValid pairs w m h s =
        case collapseWave pairs w m h s of
            Left ns -> generateUntilValid pairs w m h ns
            Right cw -> cw

    generateFromImage :: TileImg -> Int -> [Transform] -> [CoOrd] -> PureMT -> ([TileImg], Grid)
    generateFromImage input tileSize transformations shape seed =
        let (tiles, freqs) = getTileData input tileSize transformations
            rules          = processAdjacencyRules tileSize tiles
            start          = fst $ maximumBy (comparing snd) [((x,y), x*y) | (x,y) <- shape]
            heap           = H.singleton (0, start)
            startWave      = generateStartingWave shape freqs
            collapsed      = generateUntilValid rules startWave M.empty heap seed
        in
            (tiles, collapsed)

    --temporary function for testing
    testout = do
        Right x <- readPng "assets/sources/sewer.png"
        Right y <- readPng "Dungeon.png"
        let stream = convertRGB8 x
            dungeon = convertRGB8 y
            (t2, c2) = generateFromImage stream 3 withRotations (getGrid 99 99) (pureMT 69)
            (t3, c3) = generateFromImage dungeon 3 withRotations (getGrid 100 100) (pureMT 420)
            (t4, c4) = generateFromImage dungeon 3 withReflections (getGrid 200 200) (pureMT 24)
        --print "Generating 10x10 'Stream' grid..."
        --writePng "testout1.png" $ generateOutputImage (generatePixelList c1 t1) 10 10
        --print "Done"
        print "Generating 100x100 'sewer' grid..."
        writePng "testout2.png" $ generateOutputImage (generatePixelList c2 t2)
        print "Done"
        print "Generating 100x100 'Dungeon' grid..."
        writePng "testout3.png" $ generateOutputImage (generatePixelList c3 t3)
        print "Done"
        print "Generating 200x200 'Dungeon' grid..."
        writePng "testout4.png" $ generateOutputImage (generatePixelList c4 t4)
        print "Done"
