module Main where

    import qualified Data.Map as M
    import Data.Map (Map)

    import           System.Random.Mersenne.Pure64
    import           System.Random.Shuffle

    import           Codec.Picture
    import           Codec.Picture.Extra

    import DemiGen.Types
    import DemiGen.TreeGen
    import DemiGen.TileGen

    main :: IO ()
    main = do
        rooms <- allRooms
        s     <- newPureMT
        Right walls <- readPng "assets/biomes/wall.png"
        let wTiles    = convertRGB8 walls
            crooms    = choiceFromList $ zip [1,1..] rooms 
            (pop1,s1) = randomTrees 400 crooms 100 6 s
            (x,   s2) = geneticDungeon 10 (targetSize 100) pop1 rooms s1
            biomes    = parseBiomes $ embiggenDungeon x
            (tiles, cWalls) = generateFromImage wTiles 3 [] (biomes M.! Wall) s2
        print "test walls"
        writePng "testout.png" $ generateOutputImage (generatePixelList cWalls tiles) 200 200





    parseBiomes :: Dungeon -> Map Cell [CoOrd]
    parseBiomes dg = 
        M.foldlWithKey (\m coord tile -> M.insertWith (++) tile [coord] m) 
            M.empty 
            $ M.filter (/= Empty) dg

    emplaceWalls :: Dungeon -> 

    
