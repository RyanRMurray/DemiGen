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
        (S.fromList $ floor ++ doors) $ 
        [(d, Open) | d <- doors]
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
        (S.map f $ tiles) $
        L.map 
            (\(coord, c) -> (f coord, c)) 
            $ doors

    offsetRoom :: Room -> Int -> Int -> Room
    offsetRoom Room{..} ox oy = Room rType
              (S.map (\ (x,y)  -> (x+ox,y+oy)   ) tiles)
            $ L.map (\((x,y),c)->((x+ox,y+oy),c)) doors

    noCollision :: Dungeon -> Room -> CoOrd -> Bool
    noCollision d Room{..} (ox,oy) = 
        and [isFree d (x+ox,y+oy) | (x,y) <- S.toList tiles]
    
    isFree d c = d M.!? c /= Just Occupied

    findValidDoor :: Dungeon -> Room -> Maybe CoOrd
    findValidDoor d Room{..}
        | length valid /= 0 = Just $ fst $ head valid
        | otherwise         = Nothing
      where
        valid = L.filter (\d -> snd d == Open) doors
        
    insertRoom :: Dungeon -> Room -> Dungeon
    insertRoom d r =
        L.foldl (\dg c -> M.insert c Conn dg) d2 $ L.map fst $ doors r
        where
            d2 = S.foldl (\dg c -> M.insert c Occupied dg) d (tiles r)

    --Take a dungeon and a pair of rooms, if it is possible to attach the child room to the parent, return the updated dungeon
    insertChildRoom :: Dungeon -> (Room, Int) -> (Room, Int) -> Maybe (Dungeon, Room, Room)
    insertChildRoom d (parent, pid) (child, cid) = do
        (tx,ty) <- findValidDoor d parent
        let rotations     = [rotateRoom child rot | rot <- [0..3]]
            offsets       = [offsetRoom (setConn r pid (x,y)) (tx-x) (ty-y) | r <- rotations, ((x,y),_) <- doors r]
        (newD, newChild) <- msum [attemptAttach d r | r <- offsets]        
        Just (newD, setConn parent cid (tx,ty), newChild)


    attemptAttach :: Dungeon -> Room -> Maybe (Dungeon, Room)
    attemptAttach d r 
        | noCollision d r (0,0) = Just (insertRoom d r, r)
        | otherwise             = Nothing

    setConn ::  Room -> Int -> CoOrd -> Room
    setConn Room{..} toID door =
        Room rType tiles $ (door, To toID) : [(c, t) | (c,t) <- doors, c /= door]
          
--random tree generator
---------------------------------------------------------------------------------------------------

    randomChildren :: [Room] -> [(Int,Int)] -> Int -> (DungeonTree, PureMT) -> (DungeonTree, PureMT)
    randomChildren _ [] _ result = result
    randomChildren rooms ((id,budget):cs) maxChildren (res, s) =
        randomChildren rooms cs maxChildren ((M.union res subTree), s2)
      where
        (subTree, s2) = randomTree rooms id budget maxChildren s

    generateIDs :: Int -> ([Int], PureMT) -> ([Int], PureMT)
    generateIDs 0 res = res
    generateIDs cnum (res,s) = 
        generateIDs (cnum-1) (id:res, s2)
      where
        (id, s2) = randomInt s

    --given the number of children a node has, randomly distribute the budget used to generate the subtrees
    distributeBudget :: Int -> Int -> ([Int], PureMT) -> ([Int], PureMT)
    distributeBudget 0 _ result = result
    distributeBudget budget 1 (res, seed) = (budget:res, seed)
    distributeBudget budget recipients (distributed, seed) = 
        let (seedRes, seed2) = randomInt seed
            newDis           = 1 + (seedRes `mod` budget)
            remaining        = budget - newDis
        in
            distributeBudget remaining (recipients - 1) (newDis:distributed, seed2)
    
    randomTree :: [Room] -> Int -> Int -> Int -> PureMT -> (DungeonTree, PureMT)
    randomTree rooms rootID 1 _ s = 
        (M.singleton rootID (Node r []), snd $ randomInt s)
      where
        r = head $ shuffle' rooms (length rooms) s

    randomTree rooms rootID budget maxChildren s = 
        (tree, (snd $ randomInt s6))
      where
        (id, s2) = randomInt s
        (cnum, s3) = (\(a,b) -> (1 + a `mod` maxChildren, b)) $ randomInt s2 
        (cBudgets, s4) = distributeBudget (budget-1) cnum ([], s3)
        (cIDs,     s5) = generateIDs cnum ([],s4)
        (children, s6) = randomChildren rooms (zip cIDs cBudgets) maxChildren (M.empty, s5)
        r              = head $ shuffle' rooms (length rooms) s6
        tree           = M.union children $ M.singleton rootID (Node r cIDs)

    randomTrees :: [Room] -> Int -> Int -> PureMT -> ([DungeonTree], PureMT)
    randomTrees rooms 1 budget s =
        ([t], s2)
      where
        (t,s2) = randomTree rooms 0 budget 5 s

    randomTrees rooms num budget s =
        (t:ts,s3)
      where
        (t,s2) = randomTree rooms 0 budget 5 s
        (ts,s3) = randomTrees rooms (num-1) budget s2 


--tree mutation functions
---------------------------------------------------------------------------------------------------

    crossover :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    crossover _ donor recipient s =
        (crossed, snd $ randomInt s)
      where
        did    = head $ shuffle' (filter (/=0) $ M.keys donor) (length $ M.keys donor) s
        rid    = head $ shuffle' (filter (/=0) $ M.keys recipient) (length $ M.keys recipient) s
        toAdd  = getSubTree donor did
        addNode = toAdd M.! did
        crossed = 
            M.insert rid addNode
            $ M.union (M.delete did toAdd) recipient

    grow :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    grow choices _ recipient s =
        ( foldl' (\rx (i,a) -> M.insert i a rx) r2 $ zip ids toAdd
        , snd $ randomInt $ snd $ randomInt s2
        )
      where
        possible  = M.keys recipient
        (ids, s2) = generateIDs 4 ([],s)
        toAdd     = map (\a -> Node a []) $ take 4 $ shuffle' choices (length choices) s2
        toGrow    = head $ shuffle' possible (length possible) $ snd $ randomInt s2
        r2        = alterConns recipient toGrow ids

    trim :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    trim _ _ recipient s =
        (M.delete toRemove recipient, snd $ randomInt s)
      where
        leaves = M.keys $ M.filter (\(Node _ c) -> c == []) recipient
        toRemove = head $ shuffle' leaves (length leaves) s

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
        (muts4, s5) = randomFrom [(muts2 ++ replicate 2 change, 5), (muts2, 5)] s4

--tree to dungeon functions
---------------------------------------------------------------------------------------------------

    resolveTree :: RoomType -> DungeonTree -> DungeonTree -> Dungeon -> Int -> [Int] -> [Int] -> DungeonTree
    resolveTree deadend _ out _ _ [] [] = out

    resolveTree deadend input out dg _ [] (n:ns) =
        resolveTree deadend input out dg n nextCs ns
      where
        nextCs = getConns input n

    resolveTree deadend input output dg p (c:cs) next =
          case insertChildRoom dg (proom, p) (croom, c) of
              Nothing -> resolveTree deadend input output dg p cs next
              Just (d2, p2, c2) -> resolveTree deadend input o2 d2 p cs (c:next)
                where
                  o2 =  alterConns 
                          (M.insert c (Node c2 (getConns input c)) output)
                          p [c]
      where
        proom = fromJust $ msum [getRoom output p, getRoom input p]
        croom = trace ((++) ((++) (show c) " ") $ show $input M.! p) $ fromJust $ msum [getRoom output c, getRoom input c]

    treeToGenome :: RoomType -> DungeonTree -> DungeonTree
    treeToGenome deadend t =
        resolveTree deadend t startOu startDG 0 startCs []
      where
        startDG = insertRoom M.empty $ room $ t M.! 0
        startCs = getConns t 0
        startOu = M.insert 0 (Node (fromJust $ getRoom t 0) []) M.empty

    --take a set of rooms and create a Dungeon
    genomeToDungeon :: DungeonTree -> Dungeon
    genomeToDungeon =
        M.foldl' (\d (Node r _) -> insertRoom d r) M.empty

--various fitness functions
---------------------------------------------------------------------------------------------------
    
    geneticDungeon :: Int -> (DungeonTree -> Int) -> [DungeonTree] -> [Room] -> PureMT -> (DungeonTree, PureMT)
    geneticDungeon 1 f !pop _ s = (best, s)
      where
        (best,_) = last $ sortOn snd [(t, f t) | t <- pop]

    geneticDungeon gens f !pop rooms s =
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
        (first:second:_) = (map fst) . reverse $ sortOn snd [(t, scoreF t) | t <- competitors]
        (child1,s2)      = mutate rooms first second s
        (child2,s3)      = mutate rooms second first s2


    targetSize :: Int -> DungeonTree -> Int
    targetSize target t
        | s < target = s
        | otherwise  = 0
      where
        s = M.size $ treeToGenome None t


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

    addWallsToSeg :: Room -> CoOrd -> Dungeon
    addWallsToSeg Room{..} at =
        M.mapKeys (\c -> (.+) (at .* 4) c)
        $ foldl' (\s c -> M.insert c Wall s) (baseSeg Floor) toAdd
      where
        toAdd = concat 
            $ map (side . fst) 
            $ filter (\(_,c) -> S.notMember (at .+ c) tiles)
            $ zip [1..] $ [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

    addEmbiggenedRoom :: Dungeon -> Room -> Dungeon
    addEmbiggenedRoom dg r@Room{..} =
        foldl' (\d s -> M.union s d) dg
        $ map (addWallsToSeg r) 
        $ S.toList tiles

    unsealDoor :: Dungeon -> CoOrd -> Dungeon
    unsealDoor small at =
        M.mapKeys (\c -> (.+) (at .* 4) c)
        $ foldl' (\s c -> M.insert c Wall s) (baseSeg Door) toAdd
      where
        toAdd = concat 
            $ map side
            $ (++) [2,4,6,8]
            $ map fst
            $ filter (\(_,c) -> M.findWithDefault Empty (c .+ at) small == Empty)
            $ zip [1,3..] $ [(0,-1),(1,0),(0,1),(-1,0)]

    unsealDoors :: Dungeon -> Dungeon -> Dungeon
    unsealDoors sealed ref =
        foldl' (\d s -> M.union s d) sealed
        $ map (unsealDoor ref) doors
      where
        doors   = M.keys $ M.filter (== Conn) ref
        
    embiggenDungeon' :: DungeonTree -> Dungeon
    embiggenDungeon' genome =
        unsealDoors sealed dungeon
      where
        dungeon = genomeToDungeon genome
        sealed  = foldl' (\d (Node r _) -> addEmbiggenedRoom d r) M.empty genome


    embiggenDungeon :: RoomType -> DungeonTree -> Dungeon
    embiggenDungeon deadend tree =
        unsealDoors sealed dungeon
      where
        genome  = treeToGenome deadend tree
        dungeon = genomeToDungeon genome
        sealed  = foldl' (\d (Node r _) -> addEmbiggenedRoom d r) M.empty genome


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
    printDungeonPixel Empty = PixelRGB8 255 255 255
    printDungeonPixel Wall = tilePixel
    printDungeonPixel Floor = PixelRGB8 100 100 100

    test = do
        rooms <- allRooms
        s     <- newPureMT
        let (pop1,s1) = randomTrees rooms 400 100 s
            (x,   sx) = geneticDungeon 10 (targetSize 100) pop1 rooms s1
        printRawDungeon "dungeonRawOut.png" $ embiggenDungeon None x
