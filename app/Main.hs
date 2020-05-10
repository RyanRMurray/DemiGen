{-# LANGUAGE RecordWildCards #-}
module Main where

    import qualified Data.Heap as H
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.Map (Map)
    import Data.Set (Set)
    import Data.List
    import           Data.Ord

    import Text.Read
    import Data.Maybe

    import Data.Word
    import Control.Exception
    import Data.Time.Clock.System
    import Data.Bits.Extras
    import System.Environment

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


    denseSewer = Profile
        400
        100
        40
        (density 45)
        None
        "./assets/sources/sewer_capped.png"
        withRotations

    caveSpecials = Profile
        400
        100
        40
        (specialRooms 4 20)
        Special
        "./assets/sources/cave.png"
        withRotations 

    sizeDungeon = Profile
        400
        100
        20
        (roomNumBounded 30)
        None
        "./assets/sources/dungeon.png"
        withRotations
    

    options   = [denseSewer, caveSpecials, sizeDungeon]
    
    functionOptions :: Int -> Int -> [DungeonTree -> Int]
    functionOptions a b = [density a, roomNumber a, roomNumBounded a, roomNum, specialRooms a b]

    templateOptions = ["./assets/sources/dungeon.png", "./assets/sources/sewer_capped.png", "./assets/sources/cave.png"]

    parseN :: Read a => a -> String -> a
    parseN def str = fromMaybe def $ readMaybe str


    createProfile :: IO Profile
    createProfile = do
        putStrLn templateMenuText
        t <- getLine
        putStrLn functionMenuText
        f <- getLine
        putStrLn "Enter N (default 0):"
        n <- parseN 0 <$> getLine
        putStrLn "Enter M (default 0):"
        m <- parseN 0 <$> getLine
        putStrLn "Enter Generation number (default 20):"
        gens <- parseN 20 <$> getLine
        return $ Profile 
            400
            100
            gens
            ((functionOptions n m) !! (read f))
            None
            (templateOptions !! (read t))
            withRotations


    menuText :: String
    menuText = 
        "=========================================================\n" ++
        "DEMIGEN - EVOLVING DUNGEON GENERATOR\n"                      ++
        "=========================================================\n" ++
        "Select one of the following options;\n"                      ++
        "0 - Dense Sewers\n"                                          ++
        "1 - Four Special Rooms Caves\n"                              ++
        "2 - Dungeons by Size\n"                                      ++
        "3 - Customized Dungeon\n" 

    functionMenuText = 
        "Select a Fitness function;\n"                                                                        ++
        "0 - Density: Fit N rooms into as small a footprint as possible\n"                                    ++
        "1 - Room Number: Generate a dungeon with at least N rooms\n"                                         ++
        "2 - Bounded Room Number: Generate a dungeon with exactly N rooms\n"                                  ++
        "3 - Size: Generate a dungeon with as many rooms as possible\n"                                       ++
        "4 - Special Rooms: generate a dungeon with N special rooms at least M steps away from the centre\n"

    templateMenuText = 
        "Select a thematic style;\n" ++
        "0 - Generic Dungeon\n" ++
        "1 - Sewer System\n"    ++
        "2 - Cave Network\n"


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



    main :: IO ()
    main = do
        putStrLn menuText
        selected <- getLine
        prof <- optionSelect (read selected)
        putStrLn "Enter seed (default random seed);"
        defaultSeed <- w64 . systemNanoseconds <$> getSystemTime
        seed <- parseN defaultSeed <$> getLine
        putStrLn "Enter output file name;"
        outName  <- getLine
        putStrLn $ "Generating with the seed " ++ (show seed) ++ ". This may take several minutes."
        generateFromProfile prof outName (pureMT seed)
      where
        optionSelect o
            | o /= 3    = return $ options !! o
            | otherwise = createProfile
        
