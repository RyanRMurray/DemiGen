module TileGen where
    import Util (readRational)
    import Data.List
    import Data.Maybe
    import qualified Data.Map as Map
    import Data.Map (Map)
    import Text.XML.Light
    import Data.List.Split
    import Codec.Picture
    import Control.Monad

    data Rot = N | W | S | E deriving (Show)
    data Tile = Tile 
        { tileName  :: String
        , symmetry  :: String
        , weighting :: Rational
        } deriving (Show)

    data TileConfiguration = TileConfiguration Integer Rot deriving (Show)
    data Neighbor = Neighbour
        { left     :: TileConfiguration
        , right    :: TileConfiguration
        } deriving (Show)

    data TileData = TileData
        { tiles :: [Tile]
        , validNeighbours :: [Neighbor]
        , images :: [DynamicImage]
        }
    
    type CoOrd = (Integer, Integer)
    type OutGrid = (Map CoOrd TileConfiguration)

    --Functions for importing tile data from XML files
    --Functions assume a correctly-formatted input
    ---------------------------------------------------------------------------------------------------
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
    getTileData :: FilePath -> IO (Either String TileData)
    getTileData fpath = do
        xml <- parseXMLDoc <$> readFile (fpath ++ "data.xml")
        --get tiles
        let xTiles   = elChildren $ findEl "tiles" $ findEl "set" $ fromJust xml
        let tNames   = map (justAttr "name") xTiles
        let tSymms   = map (justAttr "symmetry") xTiles
        let tWeights = map (justAttr "weight") xTiles
        let tiles    = map mkTile $ zip3 tNames tSymms tWeights
        --get neighbours
        let xNeighbors = elChildren $ findEl "neighbors" $ findEl "set" $ fromJust xml
        let lNeighbor  = map (justAttr "left") xNeighbors
        let rNeighbor  = map (justAttr "right") xNeighbors
        let neighbors  = map (mkNeighbors tNames) $ zip lNeighbor rNeighbor
        --get images
        images <- sequence <$> mapM (\n -> readImage $ fpath ++ n ++ ".png") tNames 
        case images of
            Left err -> return $ Left err
            Right is -> return $ Right $ TileData tiles neighbors is

    --make tile from truple
    mkTile :: (String, String, String) -> Tile
    mkTile (name, sym, weight) = Tile name sym (readRational weight)

    --make a valid neighbour pair from the name of two tiles
    mkNeighbors :: [String] -> (String, String) -> Neighbor
    mkNeighbors ts (l, r) = Neighbour (mkConfig ts (splitOn " " l)) (mkConfig ts  (splitOn " " r))

    --make a TileConfiguration from the name of the tile and an integer representing its rotation
    mkConfig :: [String] -> [String] -> TileConfiguration
    mkConfig ts [id]      = TileConfiguration (mkConfig' id ts) N
    mkConfig ts [id, "1"] = TileConfiguration (mkConfig' id ts) W
    mkConfig ts [id, "2"] = TileConfiguration (mkConfig' id ts) S
    mkConfig ts [id, "3"] = TileConfiguration (mkConfig' id ts) E

    mkConfig' id ts = toInteger $ fromJust $ elemIndex id ts

    --Functions for printing
    -------------------------------------------------------------------------------------------------
    
    gridX = 1
    gridY = 1

    testgrid = Map.fromList [((0,0), TileConfiguration 0 N),((0,1), TileConfiguration 1 N),((1,0), TileConfiguration 1 N),((1,1), TileConfiguration 0 N)]

    --extract the image IDs for each panel in the grid, with -1 defaults
    getImageIDGrid :: OutGrid -> [[Integer]]
    getImageIDGrid grid = [getImageIDLine grid y | y <- [0..gridY]]

    getImageIDLine :: OutGrid -> Integer -> [Integer]
    getImageIDLine grid y = [getImageID $ getTile grid (x,y) | x <- [0..gridX]]

    getTile :: OutGrid -> CoOrd -> TileConfiguration
    getTile grid coord = Map.findWithDefault (TileConfiguration (-1) N) coord grid
    
    getImageID :: TileConfiguration -> Integer
    getImageID (TileConfiguration i _) = i