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

    data Rot = N | W | S | E deriving (Show)
    data Tile = Tile 
        { tileName  :: String
        , symmetry  :: String
        , weighting :: Rational
        } deriving (Show)

    data TileConfiguration = TileConfiguration 
        { tid ::Integer
        , rotation :: Rot
        } deriving (Show)
    data Neighbor = Neighbor
        { left     :: TileConfiguration
        , right    :: TileConfiguration
        } deriving (Show)

    data TileData = TileData
        { tiles          :: [Tile]
        , validNeighbors :: [Neighbor]
        }
    
    type CoOrd = (Integer, Integer)
    type OutGrid = (Map CoOrd TileConfiguration)
    type TileImg = Image VS RGB Word8

    --test variables    
    gridX = 10
    gridY = 10
    tileSize = 3

    testcoords = range ((0,0), (gridX,gridY))

    testgrid = Map.fromList 
        [ ((0,0), TileConfiguration 0 N)
        , ((0,1), TileConfiguration 1 N)
        , ((1,0), TileConfiguration 1 N)
        , ((1,1), TileConfiguration 0 N)
        ]

--Functions for importing tile data from XML files
--Functions assume a correctly-formatted input
-------------------------------------------------------------------------------------
    --return Just the requested element
    findEl :: String -> Element -> Element
    findEl name parent = fromJust $ findElement (unqual name) parent
    
    --return Just the requested element. Since weightings can be blank, return default of 0 for them.
    justAttr :: String -> Element -> String
    justAttr a e = 
        case findAttr (unqual a) e of
        Just x -> x
        _      -> "0.0"

    --get tiles and neoighbours data
    getTileData :: Element -> TileData
    getTileData xml =
        let 
            --get tiles
            xTiles   = elChildren $ findEl "tiles" $ findEl "set" $ xml
            tNames   = L.map (justAttr "name") xTiles
            tSymms   = L.map (justAttr "symmetry") xTiles
            tWeights = L.map (justAttr "weight") xTiles
            tiles    = L.map mkTile $ zip3 tNames tSymms tWeights
            --get neighbors
            xNeighbors = elChildren $ findEl "neighbors" $ findEl "set" $ xml
            lNeighbor  = L.map (justAttr "left") xNeighbors
            rNeighbor  = L.map (justAttr "right") xNeighbors
            neighbors  = L.map (mkNeighbors tNames) $ zip lNeighbor rNeighbor
        in (TileData tiles neighbors)

    readPNG :: String -> IO (Either String (TileImg))
    readPNG fpath = readImageExact PNG fpath

    --make tile from truple
    mkTile :: (String, String, String) -> Tile
    mkTile (name, sym, weight) = Tile name sym (readRational weight)

    --make a valid neighbor pair from the name of two tiles
    mkNeighbors :: [String] -> (String, String) -> Neighbor
    mkNeighbors ts (l, r) = 
        Neighbor (mkConfig ts (splitOn " " l)) (mkConfig ts (splitOn " " r))

    --make a TileConfiguration from the name of the tile and an integer representing its rotation
    mkConfig :: [String] -> [String] -> TileConfiguration
    mkConfig ts [id]      = TileConfiguration (mkConfig' id ts) N
    mkConfig ts [id, "1"] = TileConfiguration (mkConfig' id ts) W
    mkConfig ts [id, "2"] = TileConfiguration (mkConfig' id ts) S
    mkConfig ts [id, "3"] = TileConfiguration (mkConfig' id ts) E

    mkConfig' id ts = toInteger $ fromJust $ elemIndex id ts

--Functions for printing
-------------------------------------------------------------------------------------
    
    defaultTile :: TileImg
    defaultTile = makeImage (tileSize,tileSize) (defaultTilePixel) :: TileImg
    defaultTilePixel (x,y) = PixelRGB 255 0 127
    
    --extract the image IDs for each panel in the grid, with -1 defaults
    getImageIDGrid :: OutGrid -> [[Integer]]
    getImageIDGrid grid = [getImageIDLine grid y | y <- [0..gridY]]

    getImageIDLine :: OutGrid -> Integer -> [Integer]
    getImageIDLine grid y = [tid $ getTile grid (x,y) | x <- [0..gridX]]

    getTile :: OutGrid -> CoOrd -> TileConfiguration
    getTile grid coord = Map.findWithDefault (TileConfiguration (-1) N) coord grid
    
    printGrid :: [TileImg] -> [[Integer]] -> TileImg
    printGrid imgs (l:ls) = foldl topToBottom 
        (printGridLine imgs l)
        [printGridLine imgs line | line <- ls]

    printGridLine :: [TileImg] -> [Integer] -> TileImg
    printGridLine imgs (id:ids) = foldl leftToRight 
        (printGridTile imgs id)
        [printGridTile imgs i | i <- ids]

    printGridTile :: [TileImg] -> Integer -> TileImg
    printGridTile imgs (-1) = defaultTile
    printGridTile imgs id   = imgs !! (fromIntegral id)

    --main function
    outputMap :: FilePath -> Integer -> Integer -> IO ()
    outputMap fpath x y = do
        --get tiles and their neighbour data
        Right xml <- checkXML <$> (parseXMLDoc <$> readFile (fpath ++ "data.xml"))
        let tData = getTileData xml
        --get image files for tiles
        Right images <- sequence <$> mapM (\n -> readPNG $ fpath ++ n ++ ".png") (L.map tileName (tiles tData)) 
        --get seed
        putStrLn "Enter a seed number:"
        seed <- getLine
        putStrLn "todo"

    checkXML :: Maybe Element -> Either String Element
    checkXML file = 
        case file of
            Just xml -> Right xml
            _        -> Left "Failed to load data.xml from specified folder."


    startGen :: StdGen -> [CoOrd] -> TileData -> OutGrid
    startGen seed grid tData = undefined

    generateMap :: StdGen -> [CoOrd] -> TileData -> Either StdGen OutGrid
    generateMap = undefined