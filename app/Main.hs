{-# LANGUAGE RecordWildCards #-}
module Main where

    import qualified Data.Heap as H
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.Map (Map)
    import Data.Set (Set)
    import Data.List
    import           Data.Ord
    import Data.Word

    import System.CPUTime

    import           System.Random.Mersenne.Pure64
    import           System.Random.Shuffle

    import           Codec.Picture
    import           Codec.Picture.Extra

    import DemiGen.Types
    import DemiGen.TreeGen
    import DemiGen.TileGen

    data Profile = Profile
        { population           :: Int
        , maxRooms             :: Int
        , generations          :: Int
        , fitness              :: DungeonTree -> Int
        , deadEnd              :: RoomType
        , imageInput           :: String
        , imageTransformations :: [Transform]
        }


    vbSewer = Profile
        400
        100
        200
        (valtchanBrown 100)
        Special
        "./assets/sources/sewer.png"
        withRotations

    denseSewer = Profile
        400
        100
        200
        (density 200)
        None
        "./assets/sources/cave.png"
        withRotations 

    sizeDungeon = Profile
        400
        100
        60
        roomNum
        None
        "./assets/sources/dungeon.png"
        withRotations

    options = [vbSewer, denseSewer, sizeDungeon]

    getBiomes :: Dungeon -> Map Cell [CoOrd]
    getBiomes = M.foldlWithKey' (\m c b -> M.insertWith (++) b [c] m) M.empty

    setBiomeInWave :: [CoOrd] -> Set Int -> Wave ->  Wave
    setBiomeInWave targets legal w = foldl' (\wx c -> M.insert c legal wx) w targets

    biomesFromTemplate :: Bool -> Int -> [Transform] -> [TileImg] -> TileImg -> Map Cell (Set Int)
    biomesFromTemplate hasDoor n t uniques img 
        | hasDoor   = M.fromList [(Wall, wallB),(Door, doorB), (Floor, floorB (S.union wallB doorB))]
        | otherwise = M.fromList [(Wall, wallB),(Door,doorB), (Floor, floorB wallB)]
      where
        doorSec = crop (17*n) n (2*n) (5*n) img
        wallB   = S.singleton 0
        doorB   = getSubset n t uniques doorSec
        floorB  notFloor = 
            S.difference
                (S.fromList [0.. (length uniques) - 1])
                notFloor


    generateFromProfile :: Profile -> String -> PureMT -> IO ()
    generateFromProfile Profile{..} name s1 = do
        rooms       <- allRooms
        Right input <- readPng imageInput
        --generate dungeon
        ---------------------------------------------------------------------------
        let (startPop, s2) = randomTrees rooms population maxRooms s1
            (genTree,  s3) = geneticDungeon generations fitness startPop rooms s2
            dungeon        = embiggenDungeon $ treeToGenome deadEnd genTree
            sections       = getBiomes dungeon
        --generate starting wave
        -------------------------------------------------------------------------------------------
            rules       = parseRules (convertRGB8 input) 15 imageTransformations
            sectionSets = biomesFromTemplate False 15 imageTransformations (utiles rules) (convertRGB8 input)
            wave        = M.foldlWithKey (\w b set  -> setBiomeInWave (M.findWithDefault[] b sections) set w) M.empty sectionSets
            heap        = M.foldlWithKey (\h c poss -> H.insert (getEntropy (frequencies rules) poss, c) h) H.empty wave :: EntropyHeap
            wf          = WaveFunction wave M.empty heap
            wfWalled    = foldl' (\w c -> forceTile rules w 0 c) wf $ sections M.! Wall
        --generate output
        -------------------------------------------------------------------------------------------
            collapsed    = generateUntilValid rules wfWalled s3
            vertical     = generateImage (\x y -> PixelRGB8 0 0 0) 1 15
            horizontal   = generateImage (\x y -> PixelRGB8 0 0 0 ) (15+1) 1
            squaredTiles = [below [tx', horizontal] | tx <- utiles rules, let tx' = beside [tx, vertical]]
            result       = makeImage squaredTiles collapsed 15
        writePng (name ++ ".png") result


    menuText :: String
    menuText = 
        "=========================================================\n" ++
        "DEMIGEN - EVOLVING DUNGEON GENERATOR\n"                      ++
        "=========================================================\n" ++
        "Select one of the following options;\n"                      ++
        "0 - Sewers using Valtchan-Brown inspired fitness\n"          ++
        "1 - Dense Sewers\n"                                          ++
        "2 - Dungeons by size"

    main :: IO ()
    main = do
        putStrLn menuText
        selected <- getLine
        putStrLn "Enter output file name;"
        outName  <- getLine
        putStrLn "Enter seed;"
        seed <- getLine
        generateFromProfile (options !! (read selected)) outName (pureMT $ read seed)
    
