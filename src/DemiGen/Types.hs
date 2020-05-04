{-# LANGUAGE RecordWildCards #-}
module DemiGen.Types where

    import Codec.Picture
    import Codec.Picture.Extra

    import Data.Map (Map)
    import qualified Data.Map as M
    import qualified Data.Set as S
    import Data.Heap (MinHeap)
    import Data.Set (Set)
    import Data.List
    
    import           Data.Maybe

    import           System.Random.Mersenne.Pure64
    import           System.Random.Shuffle


--Random selection

    --take a list of items and their relative frequency and return a randomly selected item and the updated PRNG
    randomFrom :: [(a,Int)] -> PureMT -> (a,PureMT)
    randomFrom list s = (choices !! (i `mod` length choices), s2)
        where
            choices = concat [replicate n c | (c,n) <-list]
            (i,s2)  = randomInt s

    randomN :: Int -> PureMT -> ([Int], PureMT)
    randomN n s =
        iterate addR ([], s) !! n
      where
        addR (ns, sx) = (\(n, s2) -> (n:ns, s2)) $ randomInt sx

    randR :: Int -> PureMT -> (Int, PureMT)
    randR bound s = (\(n,sx) -> (abs n `mod` bound, sx)) $ randomInt s

    random :: [a] -> PureMT -> (a, PureMT)
    random l s = (\(n, sx) -> (l !! n, sx)) $ randR (length l) s


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

    data Test a = Test a [Test a] deriving (Show)

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

    defaultTilePixel _ _ = PixelRGB8 0 0 0

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
    
    data RoomType = Hall | Special | Normal | None
        deriving (Show, Eq, Ord)
    
    --Think of a room as a node in a tree, where doors are connected nodes.
    data Room = Room
        { rType :: RoomType 
        , tiles :: Set CoOrd
        , doors :: [CoOrd]
        } deriving (Show)


    data Tree a = Tree a [Tree a]



    data Cell = Empty | Occupied | Conn | Wall | Floor | Door
        deriving (Show, Eq, Ord)

    type Dungeon = Map CoOrd Cell

    data Node = Node
        { room       :: Room
        , children :: Set Int
        } deriving (Show)

    type DungeonTree = Map Int Node

    data Genome = Genome
        { tree :: DungeonTree
        , dungeon :: Dungeon
        , connections :: Map CoOrd (Set Int)
        }

    alterRoom :: DungeonTree -> Int -> Room -> DungeonTree
    alterRoom t id r =
        M.insert id (Node r cons) t
      where
        (Node _ cons) = t M.! id
    

    getRoom :: DungeonTree -> Int -> Maybe Room
    getRoom t id =
        room <$> t M.!? id

    addChildren :: DungeonTree -> Int -> Set Int -> DungeonTree
    addChildren t id add =
        M.insert id (Node r (S.union cons add)) t
      where 
        (Node r cons) = t M.! id

    getChildren :: DungeonTree -> Int -> Set Int
    getChildren t id = children $ t M.! id 

    getParent :: DungeonTree -> Int -> Int
    getParent t 0 = 0
    getParent t c 
        | x == [] = c
        | otherwise = head x
      where
        x = filter (\k -> elem c (getChildren t k)) $ M.keys t

    getSubTree :: DungeonTree -> Int -> DungeonTree
    getSubTree t toGet
        | S.null children = node
        | otherwise       = M.unions $ node : (map (getSubTree t) $ S.toList children)
      where
        children = getChildren t toGet
        node     = M.singleton toGet $ t M.! toGet


    traceIDs :: DungeonTree -> [Int] -> [[Int]]
    traceIDs t tocheck
        | children == [] = [tocheck]
        | otherwise = tocheck : traceIDs t children
      where
        children = concat $ map (S.toList . getChildren t) tocheck


    reassignID :: DungeonTree -> (Int, Int) -> DungeonTree
    reassignID t (old, new)
        | M.notMember old t = t
        | otherwise =
            M.map alter
            $ M.delete old
            $ M.insert new (t M.! old) t
      where
        alter n@(Node r cs)
            | S.member old cs = Node r $ S.insert new $ S.delete old cs
            | otherwise   = n

    symDiff :: (Ord a) => Set a -> Set a -> Set a
    symDiff a b = S.difference (S.union a b) (S.intersection a b)

    replaceConns :: Map CoOrd (Set Int) -> Int -> Int -> Map CoOrd (Set Int)
    replaceConns before old new = M.map alter before
      where
        alter s
            | S.notMember old s = s
            | otherwise = S.insert new $ S.delete old s
    
    mergeInGenome :: Genome -> Int -> Int -> Genome
    mergeInGenome Genome{..} pid cid =
        Genome
            (M.insert pid (Node (Room rt nt nd) nc) $ M.delete cid tree)
            dungeon
            conn
      where
        (Node (Room rt pt pd) pc) = tree M.! pid
        (Node (Room _  ct cd) cc) = tree M.! cid 
        nt   = S.union pt ct
        nd   = S.toList $ symDiff (S.fromList pd) (S.fromList cd)
        nc   = S.union pc cc
        conn = replaceConns connections cid pid

    mergeHalls :: Genome -> Genome
    mergeHalls before@(Genome t d c)
        | toFuse == [] = before
        | otherwise    = foldl' fuseChildren before toFuse
      where
        halls = M.keys $ M.filter (\n -> Hall == (rType . room $ n)) t
        toFuse = filter (\(_,n) -> not $ S.null n) $ map (\x -> (x,hallNeighbours x)) $ halls
        hallNeighbours i = S.delete i $ 
            S.filter (\x -> Hall == (rType . room $ t M.! x))
            $ S.unions $ map (\x -> M.findWithDefault S.empty x c) (doors . room $ t M.! i)
        fuse p g c
            | M.member p (tree g) && M.member c (tree g) = mergeInGenome g p c
            | otherwise                                  = g 
        fuseChildren g (p,cs) = foldl' (fuse p) g cs

    doorPixel :: PixelRGB8
    doorPixel = PixelRGB8 255 0 0

    tilePixel :: PixelRGB8
    tilePixel = PixelRGB8 0 0 0
