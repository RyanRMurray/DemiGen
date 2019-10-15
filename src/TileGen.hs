module TileGen where
    import           Graphics.Image

    import           Control.Monad.Random
    import           System.Random

    import           Data.List as L
    import           Data.List.Split
    import           Data.Ix (range)

    import qualified Data.Map as Map
    import           Data.Map (Map)

    import           Data.Maybe

    import           Text.XML.Light
    import           Util (readRational)

    import           System.Random
    import           Control.Monad.Random as Rand

    data Rot = N | W | S | E deriving (Show)
    data SymType = X | T | I | L | Z deriving (Show)
    type TileImg = Image VS RGB Word8
    type WTile = (Tile, Rational)

    data Tile = Tile
        { name :: String
        , symmetry :: SymType
        , weight   :: Rational
        , rotation :: Rot
        , image    :: TileImg
        }

    data ValidPair = ValidPair
        { lName :: String
        , lRot  :: Rot
        , rName :: String
        , rRot  :: Rot
        } deriving (Show)
    
    type CoOrd = (Integer, Integer)
    type Wave = (Map CoOrd [Integer])
    type CollapsedWave = (Map CoOrd Integer)

    gridX = 10
    gridY = 10
    tileSize = 3

    defaultTile :: TileImg
    defaultTile = makeImage (tileSize,tileSize) (defaultTilePixel) :: TileImg
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
    readPNG fpath = readImageExact PNG fpath

    --make tile from input data
    makeTile :: (String, SymType, String, Rot, TileImg) -> Tile
    makeTile (n,s,w,r,i) = Tile n s (readRational w) r i
    
    --make rotated tiles based on symmetry type
    makeRotations :: (String, String, String, TileImg) -> [Tile]
    makeRotations (n, "X", w , i) = [makeTile (n, TileGen.X, w, N, i)]
    makeRotations (n, "T", w , i) = [makeTile (n, TileGen.T, w, r, i) | r <- [N,W,S,E]]
    makeRotations (n, "I", w , i) = [makeTile (n, TileGen.T, w, r, i) | r <- [N,W]]
    makeRotations (n, "L", w , i) = [makeTile (n, TileGen.L, w, r, i) | r <- [N,W,S,E]]
    makeRotations (n, "Z", w , i) = [makeTile (n, TileGen.T, w, r, i) | r <- [N,W]]

    --make valid pairs of neighbors based on input data
    makePair :: (String, String) -> ValidPair
    makePair (l, r) = 
        let (ln, lr) = makePair' $ splitOn " " l
            (rn, rr) = makePair' $ splitOn " " r
        in
            ValidPair ln lr rn rr

    makePair' [id]      = (id, N)
    makePair' [id, "1"] = (id, W)
    makePair' [id, "2"] = (id, S)
    makePair' [id, "3"] = (id, E)

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
            validPairs = L.map makePair $ zip lNeighbor rNeighbor
        return (rotations, validPairs)

--Functions for creating the output grid image
---------------------------------------------------------------------------------------------------
    
    makeGridImage :: [Tile] -> CollapsedWave -> TileImg
    makeGridImage ts w = foldl topToBottom 
        (makeGridRow ts w 0)
        [makeGridRow ts w y | y <- [1..gridY]]

    makeGridRow :: [Tile] -> CollapsedWave -> Integer -> TileImg
    makeGridRow ts w y = foldl leftToRight 
        (makeGridTile ts w (0,y))
        [makeGridTile ts w (x,y) | x <- [1..gridX]]

    makeGridTile :: [Tile] -> CollapsedWave -> CoOrd -> TileImg
    makeGridTile ts w c = rotateGridTile $ ts !! fromIntegral (w Map.! c)

    rotateGridTile :: Tile -> TileImg
    rotateGridTile (Tile _ _ _ N i) =           i
    rotateGridTile (Tile _ _ _ W i) = rotate270 i
    rotateGridTile (Tile _ _ _ S i) = rotate180 i
    rotateGridTile (Tile _ _ _ E i) = rotate90  i

--Core Generator Functions
---------------------------------------------------------------------------------------------------

    startingWave :: [Tile] -> [CoOrd] -> Wave
    startingWave ts cs = foldl (\c -> Map.insert c $ startingWave' ts) Map.empty cs 
    startingWave' ts   = zip [0.. length ts] [w | (Tile _ _ w _ _) <- ts]

    collapseWave :: Wave -> [ValidPair] -> StdGen -> [CoOrd] -> CollapsedWave -> Either StdGen CollapsedWave
    collapseWave _ _ _ [] cw = Right cw
    
    collapseWave input vPairs seed unvisited collapsed = do
        let
            nextCoOrd = findNextTile input unvisited
            (newTile, nextSeed) = Rand.runRand (Rand.fromList $ input Map.! nextCoOrd) seed
            updatedCW = Map.insert nextCoOrd newTile collapsed
            updateUnv = updateUnvisited input updatedCW unvisited nextCoOrd
        --todo: insert new tile, update unvisited, propagate in Wave

    findNextTile :: Wave -> [CoOrd] -> CoOrd
    findNextTile w unvisited = fst $ minimumBy (comparing snd) [(c, length $ w Map.! c)| c <- unvisited]

    updateUnvisited :: Wave -> CollapsedWave -> [CoOrd] -> CoOrd -> [CoOrd]
    updateUnvisited = undefined