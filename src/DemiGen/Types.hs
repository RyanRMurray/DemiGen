module DemiGen.Types where

    import Codec.Picture
    import Codec.Picture.Extra

    import Data.Map (Map)
    import qualified Data.Map as M
    import Data.Heap (MinHeap)
    import Data.Set (Set)
    import Data.List

    import           System.Random.Mersenne.Pure64
    import           System.Random.Shuffle


--Random selection

    type Choice a = Map Int a

    --take a list of items and their relative frequency and return a randomly selected item and the updated PRNG
    randomFrom :: [(a,Int)] -> PureMT -> (a,PureMT)
    randomFrom list s = (choices !! (i `mod` length choices), s2)
        where
            choices = concat [replicate n c | (c,n) <-list]
            (i,s2)  = randomInt s
    
    choiceFromList :: [(Int, a)] -> Choice a
    choiceFromList =
        foldl (\m (w,c) -> choiceAdd m w c) M.empty 

    choiceAdd :: Choice a -> Int -> a -> Choice a
    choiceAdd cs weight choice =
        foldl' (\m (k,v) -> M.insert k v m) cs $ zip [lower..upper] $ cycle [choice]
      where
        lower = M.size cs
        upper = lower + weight - 1

    choose :: Choice a -> PureMT -> (a, PureMT)
    choose choices seed = 
        (choices M.! (x `mod` M.size choices), s2)
      where
        (x,s2) = randomInt seed

--Simple functions for grid-based processes

    dirs :: [CoOrd]
    dirs = [(0,-1),(-1,0),(0,1),(1,0)]

    getGrid :: Int -> Int -> [CoOrd]
    getGrid x y = concat [getGrid' x row| row <- [0..y]]
    getGrid' x row = [(col,row) | col <- [0..x]]

    getBounds :: [CoOrd] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
    getBounds [] res = res
    getBounds ((x,y):cs) (minx,miny,maxx,maxy) =
        getBounds cs (nminx, nminy, nmaxx, nmaxy)
      where
        nminx = min x minx
        nmaxx = max x maxx
        nminy = min y miny
        nmaxy = max y maxy

--Types and globals for TileGen
    type Pix = PixelRGB8
    type TileImg = Image Pix                        --Type of image to import and export

    type TileFreqs = Map Int Int           --Set of tile indexes with their relative rarity
    type CoOrd = (Int, Int)                         --Simple cartesian integer coordinates
    
    (.+) :: CoOrd -> CoOrd -> CoOrd
    (.+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)

    (.*) :: CoOrd -> Int -> CoOrd
    (.*) (x,y) m = (m*x,m*y)
    
    type Wave = Map CoOrd (Set Int)                 --Un or partially collapsed wavefunction containing a grid of possible tile indexes
    type Grid = Map CoOrd Int                  --Fully collapsed wavefunction containing a grid of tile indexes
    type Neighbor = (CoOrd,CoOrd)                   --Coordinates of a neighboring tile and its relative direction
    type EntropyHeap = MinHeap (Double, CoOrd)    --Heap listing partially collapsed cells by their relative entropy

    type Transform = TileImg -> TileImg             --Transformations for enriching input tile samples

    defaultTilePixel = PixelRGB8 255 0 127

    --Options for enriching input tile data
    noTransform :: Transform
    noTransform img = img

    withRotations :: [Transform]
    withRotations =
        [ noTransform
        , rotateLeft90
        , rotateRight90
        , rotate180
        ]

    withReflections :: [Transform]
    withReflections =
        [ noTransform
        , flipHorizontally
        , flipVertically
        ]

    withReflectionsAndRotations :: [Transform]
    withReflectionsAndRotations =
        [ noTransform
        , rotateLeft90
        , rotateRight90
        , rotate180
        , flipHorizontally
        , flipVertically
        ]

    withLoops :: [Transform]
    withLoops = [repeatPattern]

    repeatPattern :: Transform
    repeatPattern p = below [beside [p, p], beside [p, p]]

--types and globals for TreeGen
    --Think of a room as a node in a tree, where doors are connected nodes.
    data Room = Room
        { tiles :: Set CoOrd
        , doors :: [(CoOrd, Connection Room)]
        } deriving (Show)

    data Connection a = Blocked | Open | To a deriving (Show)

    instance Eq (Connection a) where
        (==) Blocked Blocked = True
        (==) Open Open       = True
        (==) _ _             = False

    data Cell = Empty | Occupied | Conn | Wall | Floor
        deriving (Show, Eq, Ord)

    type Dungeon = Map CoOrd Cell

    data DungeonTree = Leaf Room
                     | Node Room Int [DungeonTree]
                     | Null
                     deriving (Show)

    instance Eq DungeonTree where
        (==) Null Null = True
        (==) _ _       = False
    
    cons :: DungeonTree -> [DungeonTree] -> [DungeonTree]
    cons Null n = n
    cons a b    = a:b




    doorPixel :: PixelRGB8
    doorPixel = PixelRGB8 255 0 0

    tilePixel :: PixelRGB8
    tilePixel = PixelRGB8 0 0 0
