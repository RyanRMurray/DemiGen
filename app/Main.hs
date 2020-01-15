module Main where

    import qualified Data.Heap as H
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.Map (Map)
    import Data.Set (Set)
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
        Right input <- readPng "./assets/sources/sewer.png"
        let crooms         = choiceFromList $ zip [1,1..] rooms
            (pop1, s1)     = randomTrees 100 crooms 10 6 s
            (dt,    s2)    = geneticDungeon 10 (targetSize 20) pop1 rooms s1
            dg             = embiggenDungeon dt
            sections       = getBiomes dg
            rules          = parseRules (convertRGB8 input) 3 withRotations
            tis            = [0..(length $ utiles rules) - 1]
            baseWave       = generateStartingWave (M.keys dg) $ S.fromList tis
            wave           = filterWave (sections M.! Floor) (S.singleton 0) baseWave
            heap           = foldl' (\h c -> H.insert (getEntropy (frequencies rules) $ wave M.! c, c) h) H.empty $ M.keys wave
            wf             = foldl' (\w at -> forceTile rules w 0 at) (WaveFunction wave M.empty heap) $ sections M.! Wall
            collapsed = generateUntilValid rules wf s2
        writePng "test1.png" $ makeImage rules collapsed 3

    getBiomes :: Dungeon -> Map Cell [CoOrd]
    getBiomes = M.foldlWithKey' (\m c b -> M.insertWith (++) b [c] m) M.empty

    filterWave ::[CoOrd] -> Set Int -> Wave ->  Wave
    filterWave targets illegal w = foldl' (\wx c -> M.adjust (flip S.difference illegal) c wx) w targets




    
