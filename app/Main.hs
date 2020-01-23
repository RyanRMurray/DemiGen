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
            dg             = embiggenDungeon None dt
            biomes         = getBiomes dg
            rules          = parseRules (convertRGB8 input) 3 withRotations
            biomeSets      = biomesFromTemplate False 3 withRotations (utiles rules) (convertRGB8 input)
            wave           = M.foldlWithKey (\w b set -> setBiomeInWave (M.findWithDefault [] b biomes) set w) M.empty biomeSets
            heap           = M.foldlWithKey (\h c poss -> H.insert (getEntropy (frequencies rules) poss,c) h) H.empty wave :: EntropyHeap
            wf             = WaveFunction wave M.empty heap
            walled         = foldl' (\w c -> forceTile rules w 0 c) wf $ biomes M.! Wall
            collapsed = generateUntilValid rules walled s2
        writePng "test1.png" $ makeImage rules collapsed 3

    getBiomes :: Dungeon -> Map Cell [CoOrd]
    getBiomes = M.foldlWithKey' (\m c b -> M.insertWith (++) b [c] m) M.empty

    setBiomeInWave :: [CoOrd] -> Set Int -> Wave ->  Wave
    setBiomeInWave targets legal w = foldl' (\wx c -> M.insert c legal wx) w targets

    biomesFromTemplate :: Bool -> Int -> [Transform] -> [TileImg] -> TileImg -> Map Cell (Set Int)
    biomesFromTemplate hasDoor n t uniques img 
        | hasDoor   = M.fromList [(Wall, wallB),(Door, doorB), (Floor, floorB (S.union wallB doorB))]
        | otherwise = M.fromList [(Wall, wallB),(Door,doorB), (Floor, floorB wallB)]
      where
        wallSec = crop 0 0 n n img
        doorSec = crop (17*n) n (2*n) (5*n) img
        wallB   = getSubset n t uniques wallSec
        doorB   = getSubset n t uniques doorSec
        floorB  notFloor = 
            S.difference
                (S.fromList [0.. (length uniques) - 1])
                notFloor

    
