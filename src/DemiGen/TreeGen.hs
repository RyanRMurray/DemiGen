{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module DemiGen.TreeGen where
    import          DemiGen.Types

    import           Data.List as L
    import qualified Data.Map as M
    import qualified Data.Set as S
    import           Data.Map (Map)
    import           Data.Set (Set)

    import           System.Random.Mersenne.Pure64
    import           System.Random.Shuffle

    import           Control.Monad

    import           Data.Ord
    import           Data.Either
    import           Data.Maybe

    import           Codec.Picture
    import           Codec.Picture.Extra

    import Debug.Trace

    roomNames :: [String]
    roomNames =
        [ "hall01", "hall02", "hall03"
        , "room01", "room02", "room03", "room04", "room05", "room06", "room07", "room08", "room09", "room10"
        , "special01", "special02", "special03"
        ]

    halls = ["hall01", "hall02", "hall03"]
    rooms = ["room01", "room02", "room03", "room04", "room05", "room06", "room07", "room08", "room09", "room10"]
    special = ["special01", "special02", "special03"]


    allRooms :: IO [Room]
    allRooms = do
        iH  <- mapM (\n-> readPng $ "assets/rooms/"++n++".png") halls
        iR  <- mapM (\n-> readPng $ "assets/rooms/"++n++".png") rooms
        iS  <- mapM (\n-> readPng $ "assets/rooms/"++n++".png") special
        let convH = map (getRoomData Hall) $ L.map convertRGB8 $ rights $ iH
            convR = map (getRoomData Normal) $ L.map convertRGB8 $ rights $ iR
            convS = map (getRoomData Special) $ L.map convertRGB8 $ rights $ iS
        return (convH ++ convR ++ convS)

--Functions for generating input room templates and starting dungeon
---------------------------------------------------------------------------------------------------

    --Parse an image to create a room object
    getRoomData :: RoomType -> TileImg -> Room
    getRoomData rtype input = Room rtype
        (S.fromList $ floor) $ 
        doors
      where
        floor = getTargetPixels input tilePixel
        doors = getTargetPixels input doorPixel

    --Get a list of the coordinates of the specified pixel colour
    getTargetPixels :: TileImg -> PixelRGB8 -> [CoOrd]
    getTargetPixels input px = filter
        (\(x,y)-> px == pixelAt input x y) $
        getGrid 12 12

    --Generate an empty dungeon of the specified size (can be empty)
    generateDungeonGrid :: Int -> Int -> Map CoOrd Cell
    generateDungeonGrid x y = M.fromList
        [(c, Empty) | c <- getGrid x y]

--Functions for finding valid placements of rooms and inserting them into the dungeon
---------------------------------------------------------------------------------------------------

    rotateRoom :: Room -> Int -> Room
    rotateRoom r 0 = r
    rotateRoom r 1 = rotateRoom' r (\(x,y) -> (y, 12 - x))       
    rotateRoom r 2 = rotateRoom' r (\(x,y) -> (12 - x, 12 - y)) 
    rotateRoom r 3 = rotateRoom' r (\(x,y) -> (12 - y, x))      

    rotateRoom' Room{..} f = Room rType
        (S.map f tiles) $
        (L.map f doors) 

    offsetRoom :: Room -> Int -> Int -> Room
    offsetRoom Room{..} ox oy = Room rType
        (S.map (\(x,y) -> (x+ox,y+oy)) tiles)
        (L.map (\(x,y) -> (x+ox,y+oy)) doors)

    noCollision :: Dungeon -> Room -> CoOrd -> Bool
    noCollision d Room{..} (ox,oy) = 
        and [M.notMember (x+ox,y+oy) d| (x,y) <- S.toList tiles]
    
        
    insertRoom :: Dungeon -> (Room, (Maybe CoOrd)) -> Dungeon
    insertRoom dg (r,Nothing) = S.foldl (\dg c -> M.insert c Occupied dg) dg (tiles r)

    insertRoom dg (r, Just d) =
          M.insert d Conn d2
        where
            d2 = S.foldl (\dg c -> M.insert c Occupied dg) dg (tiles r)


    insertChildRoom :: Genome -> (Room, Int) -> (Room, Int) -> Maybe Genome
    insertChildRoom Genome{..} (parent, pid) (child, cid) = do
        let rotations = [rotateRoom child rot | rot <- [0..3]]
            offsets   = [ (offsetRoom r (px-cx) (py-cy), Just (px,py)) 
                        | r <- rotations
                        , (cx,cy) <- doors r
                        , (px,py) <- doors parent
                        , dungeon M.!? (cx,cy) /= Just Occupied
                        ]
        (newD, newChild) <- msum $ map (attemptAttach dungeon) offsets
        let conn      = head $ L.intersect (doors parent) (doors newChild)
        Just $ Genome
            (M.insert cid (Node newChild S.empty) $ addChildren tree pid (S.singleton cid))
            newD
            (foldl' (\c d -> M.insertWith (S.union) d (S.singleton cid) c) connections $ doors newChild)


    attemptAttach :: Dungeon -> (Room, Maybe CoOrd) -> Maybe (Dungeon, Room)
    attemptAttach d (r,at) 
        | noCollision d r (0,0) = Just (insertRoom d (r,at), r)
        | otherwise             = Nothing

          
--random tree generator
---------------------------------------------------------------------------------------------------

    randomChildren :: [Int] -> [Room] -> [Int] -> Int -> (DungeonTree, PureMT) -> (DungeonTree, PureMT)
    randomChildren _ _ [] _ res = res
    randomChildren ids rooms (budget:cs) maxChildren (res, s) =
        randomChildren i2 rooms cs maxChildren ((M.union res subTree), s2)
      where
        (i1,i2)       = splitAt budget ids
        (subTree, s2) = randomTree i1 rooms budget maxChildren s

    --given the number of children a node has, randomly distribute the budget used to generate the subtrees
    distributeBudget :: Int -> Int -> PureMT -> ([Int], PureMT)
    distributeBudget budget recipients s
        | budget == recipients = (M.elems base, s)
        | otherwise            = (M.elems added, s2)
      where
        base    = M.fromList $ zip [0..] $ replicate recipients 1
        (randoms, s2) = randomN (budget - recipients) s
        toAdd = map (\x -> (abs x) `mod` recipients) randoms
        added = foldl' (\m k -> M.insertWith (+) k 1 m) base toAdd

    randomTree :: [Int] -> [Room] -> Int -> Int -> PureMT -> (DungeonTree, PureMT)
    randomTree [id] rooms 1 _ s = 
        (M.singleton id (Node r S.empty), s2)
      where
        (r, s2) = random rooms s

    randomTree (i1:i2) rooms budget maxChildren s = 
        (tree, s5)
      where
        mcnum          = min maxChildren (budget-1)
        (cnum, s2)     = (\(a,b) -> (1 + a `mod` mcnum, b)) $ randomInt s
        (cBudgets, s3) = distributeBudget (budget-1) cnum s2
        cids           = init $ scanl (+) (i1 + 1) cBudgets
        (children, s4) = randomChildren i2 rooms cBudgets maxChildren (M.empty, s3)
        (r, s5)        = random rooms s4
        tree           = M.union children $ M.singleton i1 (Node r $ S.fromList cids)

    randomTrees :: [Room] -> Int -> Int -> PureMT -> ([DungeonTree], PureMT)
    randomTrees rooms 1 budget s =
        ([t], s2)
      where
        (t,s2) = randomTree [0..] rooms budget 5 s

    randomTrees rooms num budget s =
        (t:ts,s3)
      where
        (t,s2) = randomTree [0..] rooms budget 5 s
        (ts,s3) = randomTrees rooms (num-1) budget s2 


--tree mutation functions
---------------------------------------------------------------------------------------------------

    crossover :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    crossover _ donor recipient s 
        | (M.size donor) > 3 && (M.size recipient) > 3 = (purgeUnused updated, s3)
        | otherwise = (recipient, s)
      where
        (dNode, s2) = random (tail $ M.keys donor) s
        (rNode, s3) = random (tail $ M.keys recipient) s2
        toAdd       = getSubTree donor dNode
        toDelete    = getSubTree recipient rNode
        idStart     = max (fst $ M.findMax donor) (fst $ M.findMax recipient)
        swapIDs     = tail $ zip (M.keys toAdd) [idStart + 1..]
        swapped     = foldl' reassignID toAdd swapIDs
        updated     = flip 
            M.union 
              (reassignID swapped (dNode, rNode)) 
              (M.difference recipient toDelete)


    grow :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    grow choices _ recipient s = 
        ( foldl' (\rx (i,a) -> M.insert i a rx) r2 $ zip ids toAdd
        , snd $ randomInt $ snd $ randomInt s
        )
      where
        possible  = M.keys recipient
        ids       = take 4 [(fst $ M.findMax recipient) + 1..]
        toAdd     = map (\a -> Node a S.empty) $ take 4 $ shuffle' choices (length choices) s
        toGrow    = head $ shuffle' possible (length possible) $ snd $ randomInt s
        r2        = addChildren recipient toGrow $ S.fromList ids

    trim :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    trim _ _ recipient s = 
        ( M.insert parent updated
          $ M.delete chosen recipient
        , s2
        )
      where
        leaves = M.keys $ M.filter (\(Node _ cs) -> length cs == 0) recipient
        (chosen, s2) = random leaves s
        parent = getParent recipient chosen
        updated = (\(Node r cs) -> Node r (S.delete chosen cs)) $ recipient M.! parent

    change :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    change choices _ recipient s =
        (alterRoom recipient toChange r, snd . randomInt . snd $ randomInt s)
      where
        r        = head $ shuffle' choices (length choices) s
        toChange = head $ shuffle' (M.keys recipient) (M.size recipient) (snd $ randomInt s)

    mutate :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    mutate rooms donor recipient s
        | length muts4 == 0 = change rooms donor recipient s
        | otherwise         = foldl' (\(r,sx) f -> f rooms donor r sx) (recipient, s5) muts4
      where
        (muts,  s2) = randomFrom [([crossover], 7), ([], 3)] s
        (muts2, s3) = randomFrom [(muts ++ replicate 3 grow, 5), (muts, 5)] s2
        (muts3, s4) = randomFrom [(muts2 ++ [trim], 5), (muts2, 5)] s3
        (muts4, s5) = randomFrom [(muts3 ++ replicate 2 change, 5), (muts3, 5)] s4

--tree to dungeon functions
---------------------------------------------------------------------------------------------------

    purgeDoors :: Genome -> Genome
    purgeDoors Genome{..} =
        Genome tree dungeon used
      where
        used = M.filter (\x -> S.size x /= 1) $ connections
        --purged = M.filterWithKey (\k v -> v /= Conn || M.member k used) dungeon


    purgeUnused :: DungeonTree -> DungeonTree
    purgeUnused t =
        M.map (\(Node r cs) -> Node r $ S.intersection cs nodes) t
      where
        nodes   = S.fromList $ M.keys t

    resolveTree :: RoomType -> DungeonTree -> Genome -> Int -> [Int] -> [Int] -> Genome
    resolveTree deadend _ out _ [] [] = out

    resolveTree deadend input out _ [] (n:ns)
        | rt == deadend = resolveTree deadend input out n [] ns
        | otherwise     = resolveTree deadend input out n nextCs ns
      where
        rt     = rType $ room (input M.! n)
        nextCs = S.toList $ getChildren input n

    resolveTree deadend input output p (c:cs) next =
          case insertChildRoom output (proom, p) (croom, c) of
              Nothing -> resolveTree deadend input output p cs next
              Just o2 -> resolveTree deadend input o2 p cs (c:next)
      where   
        proom = fromJust $ getRoom (tree output) p
        croom = fromJust $ getRoom input c

    treeToGenome :: RoomType -> DungeonTree -> Genome
    treeToGenome deadend t = purgeDoors $ res
      where
        startDG = insertRoom M.empty $ (room $ t M.! 0, Nothing)
        startChildren = S.toList $ getChildren t 0
        startOTree    = M.insert 0 (Node (room $ t M.! 0) S.empty) M.empty
        startConns    = foldl' (\m x -> M.insert x (S.singleton 0) m) M.empty $ doors $ room $ t M.! 0
        res     = resolveTree deadend t (Genome startOTree startDG startConns) 0 startChildren []


--various fitness functions
---------------------------------------------------------------------------------------------------
    
    geneticDungeon :: Int -> (DungeonTree -> Int) -> [DungeonTree] -> [Room] -> PureMT -> (DungeonTree, PureMT)
    geneticDungeon 1 f !pop _ s = (best, s)
      where
        (best,_) = last $ sortOn snd [(t, f t) | t <- pop]

    geneticDungeon gens f !pop rooms s = trace (show gens)
        geneticDungeon (gens-1) f nextPop rooms s2
      where
        (nextPop, s2) = generation f pop rooms s


    generation ::  (DungeonTree -> Int) -> [DungeonTree] -> [Room] -> PureMT -> ([DungeonTree],PureMT)
    generation f population rooms s =
        (newPop,s2)
      where
        shuffled      = shuffle' population (length population) s
        tourneys      = tourneys' shuffled
        (newPop,s2)   = foldl' (\(d,sx) t -> foldTournaments f d t rooms sx) ([],s) tourneys

    tourneys' :: [DungeonTree] -> [[DungeonTree]]
    tourneys' p
        | length p /= 4 = t : tourneys' ts
        | otherwise     = [p]
      where
        (t,ts) = splitAt 4 p

    foldTournaments ::  (DungeonTree -> Int) -> [DungeonTree] -> [DungeonTree] -> [Room] -> PureMT -> ([DungeonTree], PureMT)
    foldTournaments f done next rooms s =
        (res ++ done,s2)
      where
        (res, s2) = tournament f next rooms s

    tournament ::  (DungeonTree -> Int) -> [DungeonTree] -> [Room] -> PureMT -> ([DungeonTree],PureMT)
    tournament scoreF competitors rooms s = 
        ([first, second, child1, child2],s3)
      where
        !(first:second:_) = (map fst) . reverse $ sortOn snd [(t, scoreF t) | t <- competitors]
        !(child1,s2)      = mutate rooms first second s
        !(child2,s3)      = mutate rooms second first s2


    targetSize :: Int -> DungeonTree -> Int
    targetSize target t
        | s < target = s
        | otherwise  = 0
      where
        s = M.size $ tree $ treeToGenome None t


    valtchanBrown :: Int -> DungeonTree -> Int
    valtchanBrown max t
        | M.findWithDefault 0 Special nums > 3 = 0
        | M.size t > max       = 0
        | otherwise            = (M.size (tree resolved)) + (sum [scoreRoom i | i <- M.keys (tree merged)]) - (M.findWithDefault 0 Hall nums)
      where
        resolved = treeToGenome Special t
        merged   = mergeHalls resolved
        nums     = M.foldl' (\m (Node (Room rt _ _) _) -> M.insertWith (+) rt 1 m) M.empty (tree resolved)
        scoreRoom i 
            | pType == Hall && (length cTypes) < 3        = 0
            | pType == Hall                               = (*) 10 $ length $ take 5 cTypes
            | pType == Normal && normalReward             = 50
            | pType == Special  = 50
            | otherwise                                   = 0
          where
            pRoom        = room $ (tree merged) M.! i
            pType        = rType $ pRoom
            connected    = S.delete i 
                           $ foldl' (\s ci -> S.union s $ M.findWithDefault S.empty ci (connections merged)) S.empty (doors pRoom)
            cTypes       = S.toList $ S.map (\ci -> rType $ room (tree merged M.! ci)) connected
            normalReward = (filter (==Hall) cTypes) /= [] && (filter (/=Hall) cTypes) /= []


    density :: Int -> Int -> DungeonTree -> Int
    density req maxi t 
        | M.size rs < req  = M.size rs 
        | M.size rs > maxi = 0
        | otherwise        = 10000 - (area - M.size dg)
      where
        (Genome rs dg _)      = treeToGenome None t
        (minx,miny,maxx,maxy) = getBounds (M.keys dg) (-5,-5,0,0)
        area                  = (maxx + abs minx) * (maxy + abs miny)


--Prepare dungeon for room content generation
---------------------------------------------------------------------------------------------------

    side :: Int -> [CoOrd]
    side 1 = [(x,0) | x <- [0..3]]
    side 3 = [(3,y) | y <- [0..3]]
    side 5 = [(x,3) | x <- [0..3]]
    side 7 = [(0,y) | y <- [0..3]]
    side 2 = [(3,0)]
    side 4 = [(3,3)]
    side 6 = [(0,3)]
    side 8 = [(0,0)]

    baseSeg :: Cell -> Dungeon
    baseSeg cell = M.fromList [((x,y),cell) | x <- [0..3], y<- [0..3]]

    addWallsToSeg ::  Map CoOrd (Set Int) -> Room -> CoOrd ->Dungeon
    addWallsToSeg connections Room{..} at =
        M.mapKeys (\c -> (.+) (at .* 4) c)
        $ foldl' (\s c -> M.insert c Wall s) (baseSeg Floor) toAdd
      where
        usedDoors = filter (\d -> M.member d connections) doors
        toAdd = concat
          $ map (side . fst)
          $ filter (\(_,c) -> S.notMember (at .+ c) tiles && notElem (at .+ c) usedDoors)
          $ zip [1..] $ [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

    addWallsToDoor :: Set CoOrd -> CoOrd -> Dungeon
    addWallsToDoor nTiles at =
        M.mapKeys (\c -> (.+) (at .* 4) c)
        $ foldl' (\s c -> M.insert c Wall s) (baseSeg Door) toAdd
      where
        toAdd = concat 
            $ map side
            $ (++) [2,4,6,8]
            $ map fst
            $ filter (\(_,c) -> S.notMember (at .+ c) nTiles)
            $ zip [1,3..] $ [(0,-1),(1,0),(0,1),(-1,0)]

    addEmbiggenedRoom :: Genome -> Room -> Genome
    addEmbiggenedRoom Genome{..} r@Room{..} = 
        Genome tree d2 connections
      where
        d2 =
            foldl' (\d s -> M.union s d) dungeon
            $ map (addWallsToSeg connections r)
            $ S.toList tiles
    
    getConnectedRoomTiles :: DungeonTree -> Set Int -> Set CoOrd
    getConnectedRoomTiles tree ids =
        S.unions $ map (\i -> tiles $ room $ tree M.! i) $ S.toList ids

    addEmbiggenedDoors :: Genome -> Dungeon
    addEmbiggenedDoors Genome{..} = M.unions 
        $ map (\(at, ts) -> addWallsToDoor ts at)
        $ map (\(at, cs) -> (at, getConnectedRoomTiles tree cs)) $ M.toList connections

    embiggenDungeon :: Genome -> Dungeon
    embiggenDungeon g =
        M.union floor doors
      where
        floor = dungeon $ foldl' addEmbiggenedRoom (Genome (tree g) M.empty (connections g)) $ map room $ M.elems $ tree g
        doors = addEmbiggenedDoors g

--test outputs
---------------------------------------------------------------------------------------------------
    
    printBoundedRawDungeon :: Dungeon -> Int -> Int -> IO ()
    printBoundedRawDungeon d x y = writePng "dungeonRawOut.png" $
        generateImage (\x y -> printDungeonPixel $ M.findWithDefault Empty (x,y) d) x y

    printRawDungeon :: String -> Dungeon -> IO ()
    printRawDungeon name d =
            writePng name $ 
            generateImage 
                (\x y -> printDungeonPixel $ M.findWithDefault Empty (x+minx-5,y+miny-5) d) 
                (maxx + abs minx + 10) (maxy + abs miny + 10)
      where
        (minx,miny,maxx,maxy)  = getBounds (M.keys d) (-5,-5,0,0)

    printDungeonPixel :: Cell -> PixelRGB8
    printDungeonPixel Door = doorPixel
    printDungeonPixel Occupied = tilePixel
    printDungeonPixel Conn = doorPixel
    printDungeonPixel Empty = PixelRGB8 255 255 255
    printDungeonPixel Wall = tilePixel
    printDungeonPixel Floor = PixelRGB8 100 100 100

    test = do
        rooms <- allRooms
        s     <- newPureMT
        let (pop1,s1) = randomTrees rooms 400 100 s
            (x,   sx) = geneticDungeon 20 (valtchanBrown 100) pop1 rooms s1
        printRawDungeon "dungeonRawOut.png" $ embiggenDungeon $ treeToGenome Special x
