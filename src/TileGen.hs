module TileGen where
    import           Graphics.Image

    import           Data.List as L
    import           Data.List.Split
    import           Data.Ord
    import           Data.Ix (range)

    import qualified Data.Map as Map
    import           Data.Map (Map)

    import           Data.Maybe

    import           Text.XML.Light
    import           Util (readRational)

    import           System.Random
    import           Control.Monad.Random as Rand

    type Rot = Int                                    -- Specifies direction of 'top' of tile relative to 'North'
    data SymType = X | T | I | L | Z deriving (Show)  -- Specifies the type of symmetry as seen in WaveFunctionCollapse
    type TileImg = Image VS RGB Word8                 -- Type of image to import and export

    data Tile = Tile                                  -- Tiles imported from XML, both data and image
        { name :: String
        , symmetry :: SymType
        , weight   :: Rational
        , rotation :: Rot
        , image    :: TileImg
        }

    data ValidPair = ValidPair                        -- Pairs of tiles at certain rotations that are considered valid
        { lName :: String                             -- neighbours
        , lRot  :: Rot
        , rName :: String
        , rRot  :: Rot
        } deriving (Show, Eq)
    
    type CoOrd = (Int, Int)                           -- Coordinates of tiles on an (x,y) plane
    type Wave = (Map CoOrd [(Tile, Rational)])         -- An partially collapsed wave containing valid tiles for all spaces
    type CollapsedWave = (Map CoOrd Tile)              -- A collapsed wave grid with IDs for all accepted tiles

    gridX = 2
    gridY = 1
    tileSize = 3

    defaultTile :: Tile
    defaultTile = Tile "error" TileGen.X 0.0 0 (makeImage (tileSize,tileSize) defaultTilePixel :: TileImg)
    defaultTilePixel (x,y) = PixelRGB 255 0 127

--Functions for reading from xml
---------------------------------------------------------------------------------------------------

    --return Just the requested element
    findEl :: String -> Element -> Element
    findEl name parent = fromJust $ findElement (unqual name) parent

    --return Just the requested element. Since weightings can be blank, return default of 0 for them.
    justAttr :: String -> Element -> String
    justAttr a e = 
        case findAttr (unqual a) e of
        Just x -> x
        _      -> "0.000001"
    
    --get image
    readPNG :: FilePath -> IO (Either String TileImg)
    readPNG = readImageExact PNG

    --make tile from input data
    makeTile :: (String, SymType, String, Rot, TileImg) -> Tile
    makeTile (n,s,w,r,i) = Tile n s (readRational w) r i
    
    --make rotated tiles based on symmetry type
    makeRotations :: (String, String, String, TileImg) -> [Tile]
    makeRotations (n, "X", w , i) = [makeTile (n, TileGen.X, w, r, i) | r <- [0,1,2,3]]
    makeRotations (n, "T", w , i) = [makeTile (n, TileGen.T, w, r, i) | r <- [0,1,2,3]]
    makeRotations (n, "I", w , i) = [makeTile (n, TileGen.T, w, r, i) | r <- [0,1,2,3]]
    makeRotations (n, "L", w , i) = [makeTile (n, TileGen.L, w, r, i) | r <- [0,1,2,3]]
    makeRotations (n, "Z", w , i) = [makeTile (n, TileGen.T, w, r, i) | r <- [0,1,2,3]]

    --make valid pairs of neighbors based on input data
    makePair :: (String, String) -> [ValidPair]
    makePair (l, r) = 
        let (ln, lr) = makePair' $ splitOn " " l
            (rn, rr) = makePair' $ splitOn " " r
        in
            [ValidPair ln lr rn rr, ValidPair rn (rotateTowards rr 2) ln (rotateTowards lr 2)]

    makePair' [id]      = (id, 0)
    makePair' [id, "1"] = (id, 1)
    makePair' [id, "2"] = (id, 2)
    makePair' [id, "3"] = (id, 3)

    --input tile data from an XML file in the supplied directory with the name 'data.xml'
    getTileData :: FilePath -> IO ([Tile], [ValidPair])
    getTileData fpath = do
        xml    <- parseXMLDoc <$> readFile (fpath ++ "data.xml")
        let 
        --get tiles
            xTiles   = elChildren $ findEl "tiles" $ findEl "set" $ fromJust xml 
            tNames   = L.map (justAttr "name") xTiles
            tSymms   = L.map (justAttr "symmetry") xTiles
            tWeights = L.map (justAttr "weight") xTiles
        --get neighbors
            xNeighbors = elChildren $ findEl "neighbors" $ findEl "set" $ fromJust xml
            lNeighbor  = L.map (justAttr "left") xNeighbors
            rNeighbor  = L.map (justAttr "right") xNeighbors
        --get images
        Right images <- sequence <$> mapM (\n -> readPNG $ fpath ++ n ++ ".png") tNames
        let
        --generate tile data
            rotations  = concatMap makeRotations $ zip4 tNames tSymms tWeights images
            validPairs = concat $ L.map makePair $ zip lNeighbor rNeighbor
        return (rotations, validPairs)

--Functions for creating the output grid image
---------------------------------------------------------------------------------------------------
    
    makeGridImage :: CollapsedWave -> Int -> Int -> TileImg
    makeGridImage w x y =  foldl topToBottom 
        (makeGridRow w x 0)
        [makeGridRow w x row | row <- [0..y]]

    makeGridRow :: CollapsedWave -> Int -> Int -> TileImg
    makeGridRow w x row = foldl leftToRight 
        (makeGridTile w (0,row))
        [makeGridTile w (col,row) | col <- [1..x]]

    makeGridTile :: CollapsedWave -> CoOrd -> TileImg
    makeGridTile w c = rotateGridTile $ Map.findWithDefault defaultTile c w

    rotateGridTile :: Tile -> TileImg
    rotateGridTile (Tile _ _ _ 0 i) =           i
    rotateGridTile (Tile _ _ _ 1 i) = rotate270 i
    rotateGridTile (Tile _ _ _ 2 i) = rotate180 i
    rotateGridTile (Tile _ _ _ 3 i) = rotate90  i

--Core Generator Functions
---------------------------------------------------------------------------------------------------

    --generate wave with all tiles as possible candidates in all supplied coordinates
    startingWave :: [Tile] -> [CoOrd] -> Wave
    startingWave ts cs = Map.fromList [(c, startingWave' ts)| c <- cs]
    
    startingWave' ts   = [(t, weight t) | t <- ts]

    --main substructure generation function. will return the next seed upon a contradiction, or else a fully collapsed wave
    collapseWave :: Wave -> [ValidPair] -> StdGen -> [CoOrd] -> CollapsedWave -> Either StdGen CollapsedWave
    collapseWave _ _ _ [] cw = Right cw
    
    collapseWave input vPairs seed unvisited collapsed = do
        let
            nextCoOrd           = findNextTile input unvisited
            (newTile, nextSeed) = Rand.runRand (Rand.fromList $ input Map.! nextCoOrd) seed
            updatedCW           = Map.insert nextCoOrd newTile collapsed
            updatedUnv          = updateUnvisited input updatedCW unvisited nextCoOrd
        updatedWave <- collapseNeighbors vPairs nextSeed newTile (getNeighbors input nextCoOrd) input
        collapseWave updatedWave vPairs nextSeed updatedUnv updatedCW

    findNextTile :: Wave -> [CoOrd] -> CoOrd
    findNextTile w unvisited = fst $ minimumBy (comparing snd) [(c, length $ w Map.! c)| c <- unvisited]

    updateUnvisited :: Wave -> CollapsedWave -> [CoOrd] -> CoOrd -> [CoOrd]
    updateUnvisited w cw unvisited visited = delete visited $
        filter (\n -> Map.notMember n cw) (unvisited ++ L.map snd (getNeighbors w visited))

    getNeighbors :: Wave -> CoOrd -> [(Rot,CoOrd)]
    getNeighbors w (x,y) = 
            filter (\n -> Map.member (snd n) w)
                [ (0,(x,y-1))
                , (1,(x-1,y))
                , (2,(x,y+1))
                , (3,(x+1,y))
                ]

    collapseNeighbors :: [ValidPair] -> StdGen -> Tile ->  [(Rot, CoOrd)]  -> Wave -> Either StdGen Wave
    collapseNeighbors _ _ _ [] w = Right w
    collapseNeighbors pairs seed newTile (n:ns) w =
        case collapse pairs newTile n w of
            [] -> Left seed
            newPSpace -> collapseNeighbors pairs seed newTile ns (Map.insert (snd n) newPSpace w)


    collapse :: [ValidPair] -> Tile -> (Rot, CoOrd) -> Wave -> [(Tile, Rational)]
    collapse pairs newTile (rot, n) w =
        filterValidNeighbors pairs rot newTile (w Map.! n)

    filterValidNeighbors :: [ValidPair] -> Rot -> Tile -> [(Tile, Rational)] -> [(Tile, Rational)]
    filterValidNeighbors pairs rot newTile =
        filter (validAfterRotation pairs rot newTile)
    
    validAfterRotation :: [ValidPair] -> Rot -> Tile -> (Tile, Rational) -> Bool
    validAfterRotation pairs rot (Tile ln _ _ lr _) (Tile rn _ _ rr _ , _) = 
        let offset = abs 3 - rot
        in 
            elem (ValidPair ln (rotateTowards lr offset) rn (rotateTowards rr offset)) pairs

    rotateTowards :: Rot -> Rot -> Rot
    rotateTowards dir rot = mod (dir + rot) 4
    
--Misc
---------------------------------------------------------------------------------------------------
    
    generateGridCoOrds :: Int -> Int -> [CoOrd]
    generateGridCoOrds width height =
        concat [generateGridCoOrds' width y | y <- [0..height]]

    generateGridCoOrds' width y = [(x,y) | x <- [0..width]]


    generateFromInputs :: IO ()
    generateFromInputs = do
        putStrLn "Enter the path to the data directory:"
        fpath <- getLine
        (tiles, pairs) <- getTileData fpath
        putStrLn "Enter width and then height of output grid"
        x <- readLn :: IO Int 
        y <- readLn :: IO Int
        let sWave = startingWave tiles $ generateGridCoOrds x y
        print $ Map.keys sWave
        putStrLn "Enter starting coordinates, x then y"
        sx <- readLn :: IO Int
        sy <- readLn :: IO Int
        putStrLn "Enter random seed number"
        seed <- readLn :: IO Int
        let cw = generateUntilValid sWave pairs (mkStdGen seed) [(sx,sy)]
        putStrLn "created grid with panels:"
        print $ Map.keys cw 
        writeImageExact PNG [] "out.png" (makeGridImage cw 10 10)

    generateUntilValid :: Wave -> [ValidPair] -> StdGen -> [CoOrd] ->  CollapsedWave
    generateUntilValid w pairs seed start =
        case collapseWave w pairs seed start Map.empty of
            Left s -> generateUntilValid w pairs s start
            Right cw -> cw


