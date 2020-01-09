module Main where

    import qualified Data.Heap as H
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.Map (Map)
    import Data.List
    import           Data.Ord

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
        Right input <- readPng "assets/sources/sewer.png"
        let crooms         = choiceFromList $ zip [1,1..] rooms
            (pop1, s1)     = randomTrees 400 crooms 100 6 s
            (x,    s2)     = geneticDungeon 10 (targetSize 100) pop1 rooms s1
            (tiles, freqs) = getTileData (convertRGB8 input) 3 withRotations
            sections       = parseBiomes $ embiggenDungeon x
            wave           = generateStartingWave (sections M.! Floor) freqs
            rules          = processAdjacencyRules 3 tiles
            start          = fst $ maximumBy (comparing snd) [((x,y), x*y) | (x,y) <- M.keys wave]
            heap           = H.singleton (0, start)
            collapsed      = generateUntilValid rules wave M.empty heap s2
        print "Done"
        writePng "testout.png" $ generateOutputImage (generatePixelList collapsed tiles)





    parseBiomes :: Dungeon -> Map Cell [CoOrd]
    parseBiomes dg = 
        M.foldlWithKey (\m coord tile -> M.insertWith (++) tile [coord] m) 
            M.empty 
            $ M.filter (/= Empty) dg

    emplaceWalls :: Map Cell [CoOrd] -> TileFreqs -> (Wave, [CoOrd])
    emplaceWalls dg ts =
        (foldl' (\b c -> M.insert c (S.singleton (0,1)) b) base walls, shape)
      where
        walls = dg M.! Wall
        shape = M.foldl (\s cs -> s ++ cs) [] dg
        base  = foldl' (\w c -> M.insert c ts w) M.empty shape 

    
