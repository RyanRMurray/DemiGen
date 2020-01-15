{-# LANGUAGE BangPatterns #-}
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

    import           Codec.Picture
    import           Codec.Picture.Extra

    import Debug.Trace

    roomNames :: [String]
    roomNames =
        [ "hall01", "hall02", "hall03"
        , "room01", "room02", "room03", "room04", "room05", "room06", "room07", "room08", "room09", "room10"
        , "special01", "special02", "special03"
        ]

    --simple biome of all rooms
    allRooms :: IO [Room]
    allRooms = do
            inputs   <-  mapM (\n-> readPng $ "assets/rooms/"++n++".png") roomNames
            return $ map getRoomData $ L.map convertRGB8 $ rights $ inputs
    

--Functions for generating input room templates and starting dungeon
---------------------------------------------------------------------------------------------------

    --Parse an image to create a room object
    getRoomData :: TileImg -> Room
    getRoomData input = Room
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

    rotateRoom' r f = Room
        (S.map f $ tiles r) $
        L.map 
            (\(coord, c) -> (f coord, c)) 
            $ doors r

    offsetRoom :: Room -> Int -> Int -> Room
    offsetRoom (Room ts ds) ox oy = Room
            (S.map (\ (x,y)   -> (x+ox,y+oy)   ) ts) $
             L.map (\((x,y),c)->((x+ox,y+oy),c)) ds

    noCollision :: Dungeon -> Room -> CoOrd -> Bool
    noCollision d (Room ts _) (ox,oy) = 
        and [isFree d (x+ox,y+oy) | (x,y) <- S.toList ts]
    
    isFree d c = d M.!? c /= Just Occupied

    findValidDoor :: Dungeon -> Room -> Maybe CoOrd
    findValidDoor d (Room _ doors)
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
    insertChildRoom :: Dungeon -> Room -> Room -> Maybe (Dungeon, Room, Room)
    insertChildRoom d parent child = do
        (tx,ty) <- findValidDoor d parent
        let rotations     = [rotateRoom child rot | rot <- [0..3]]
            offsets       = [offsetRoom (setConn r parent (x,y)) (tx-x) (ty-y) | r <- rotations, ((x,y),_) <- doors r]
        (newD, newChild) <- msum [attemptAttach d r | r <- offsets]        
        Just (newD, setConn parent newChild (tx,ty), newChild)


    attemptAttach :: Dungeon -> Room -> Maybe (Dungeon, Room)
    attemptAttach d r 
        | noCollision d r (0,0) = Just (insertRoom d r, r)
        | otherwise             = Nothing

    setConn :: Room -> Room -> CoOrd -> Room
    setConn (Room ts doors) connected door =
        Room ts $ (door, To connected) : [(c, t) | (c,t) <- doors, c /= door]
          
--random tree generator
---------------------------------------------------------------------------------------------------

    --generate the specified number of random trees given a  set of rooms, maximum size and number of children a node can have
    randomTrees :: Int -> Choice Room -> Int -> Int -> PureMT -> ([DungeonTree], PureMT)
    randomTrees 1 r n m s = 
        ([t],s2)
      where
        (t,s2) = randomTree r n m s

    randomTrees num r n m s =
        (t:ts,s3)
      where
        (t,s2)  = randomTree r n m s
        (ts,s3) = randomTrees (num-1) r n m s2


    --generate a random tree given a  set of rooms, maximum size and number of children a node can have
    randomTree :: Choice Room -> Int -> Int -> PureMT -> (DungeonTree, PureMT)
    randomTree rooms 1 _ seed = 
        (Leaf r, s2)
      where
        (r, s2) = choose rooms seed

    randomTree rooms budget maxChildren seed =
        let (seedRes,  seed2) = randomInt seed
            childNum          = 1 + seedRes `mod` maxChildren 
            (cBudgets, seed3) = distributeBudget (budget-1) childNum ([], seed2)
            (children, seed4) = randomChildren rooms cBudgets maxChildren ([], seed3)
            (pRoom, seed5)    = choose rooms seed4
        in
            (Node pRoom budget children, seed5)

    --given the number of children a node has, randomly distribute the budget used to generate the subtrees
    distributeBudget :: Int -> Int -> ([Int], PureMT) -> ([Int], PureMT)
    distributeBudget 0 _ result = result
    distributeBudget budget 1 (res, seed) = (budget:res, seed)
    distributeBudget budget recipients (distributed, seed) =
        let (seedRes, seed2) = randomInt seed
            newDis           = 1 + seedRes `mod` budget
            remaining        = budget - newDis
        in
            distributeBudget remaining (recipients - 1) (newDis:distributed, seed2)

    randomChildren :: Choice Room -> [Int] -> Int -> ([DungeonTree], PureMT) -> ([DungeonTree], PureMT)
    randomChildren _ [] _ result = result
    randomChildren rooms (c:cs) maxChildren (res, seed) =
        randomChildren rooms cs maxChildren (newC:res, seed2)
      where
        (newC, seed2) = randomTree rooms c maxChildren seed

--tree mutation functions
---------------------------------------------------------------------------------------------------

    --mutate applies a number of transformations to new trees. 
    mutate :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    mutate rooms donor recipient s
        | length muts4 == 0 = change rooms donor recipient s
        | otherwise         = foldl' (\(r,sx) f -> f rooms donor r sx) (recipient,s5) muts4
      where
        (muts,  s2) = randomFrom [([crossover], 7), ([], 3)] s
        (muts2, s3) = randomFrom [(muts ++ replicate 3 grow, 5), (muts, 5)] s
        (muts3, s4) = randomFrom [(muts2 ++ [trim], 5), (muts2, 5)] s
        (muts4, s5) = randomFrom [(muts2 ++ replicate 2 change, 5), (muts2, 5)] s

    --transplant a subtree from one dungeon to another
    crossover :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    crossover _ donor recipient s =
        applyToRandom f recipient s2
      where
        (new,s2) = getRandomSubTree donor s
        f        = swapSubTree new

    swapSubTree :: DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    swapSubTree new host s = (new, s)

    getRandomSubTree :: DungeonTree -> PureMT -> (DungeonTree, PureMT)
    getRandomSubTree (Leaf r) s = ((Leaf r), s)
    getRandomSubTree parent s 
        | choice == -1 = (parent, s2)
        | otherwise    = getRandomSubTree (children !! choice) s2
      where
        (Node _ _ children) = parent
        choiceList        =  (-1, 1) : [(i, getSize c) | (i,c) <- zip [0..] children]
        (choice, s2)      = randomFrom choiceList s

    --increase the size of the tree
    grow :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    grow choices _ recipient s =
        applyToRandom f recipient s5
      where
        (choice1, s2) = randomFrom [(r,1)| r<-choices] s
        (choice2, s3) = randomFrom [(r,1)| r<-choices] s2
        (choice3, s4) = randomFrom [(r,1)| r<-choices] s3
        (choice4, s5) = randomFrom [(r,1)| r<-choices] s4
        chosen        = [Leaf choice1, Leaf choice2, Leaf choice3, Leaf choice4]
        f (Leaf r) sd = (Node r 5 chosen, sd)
        f (Node r  size children) sd = (Node r (size + 4) (chosen ++ children), sd) 

    --delete a leaf
    trim :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    trim _ _ = 
        applyToRandom f
      where
        f (Leaf _) s = (Null, s)
        f child    s = applyToRandom f child s
    
    --alter the type of room a node represents, possibly changing the layout of its children
    change :: [Room] -> DungeonTree -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    change choices _ recipient s =
        applyToRandom f recipient s2
      where
        (choice, s2)    = randomFrom [(r,1)| r<-choices] s
        f (Leaf r)   sd = (Leaf choice, sd)
        f (Node r size c) sd = (Node choice size c, sd)

    --take a function that transforms a tree and apply it to a random node/leaf
    applyToRandom :: (DungeonTree -> PureMT -> (DungeonTree, PureMT)) -> DungeonTree -> PureMT -> (DungeonTree, PureMT)
    applyToRandom f (Leaf r) s = f (Leaf r) s
    applyToRandom f parent s 
        | choice == -1 = f parent s2
        | otherwise    = applyToRandom' f parent choice s2
      where
        (Node _ _ children)  = parent
        choiceList          = (-1, 1) : [(i, getSize c) | (i,c) <- zip [0..] children]
        (choice, s2)        = randomFrom choiceList s
        
    --insert updated subtree into parent's children list
    applyToRandom' f (Node r size children) choice s =
        (Node r (sum $ map getSize updated) updated,s2)
      where
        (updatedChild, s2) = applyToRandom f (children !! choice) s
        (a,(b:c))          = splitAt choice children
        updated            = (++) a $ cons updatedChild c
     
    --should probably just make this a feature of a node
    getSize :: DungeonTree -> Int
    getSize (Leaf _) = 1
    getSize (Node _ s _) = s

--tree to dungeon functions
---------------------------------------------------------------------------------------------------

    --convert the tree to a list of rooms offset to their positions in the dungeon
    treeToGenome :: DungeonTree -> [Room]
    treeToGenome (Leaf (Room ts _)) = [Room ts []]

    treeToGenome (Node r size cs) = purgeDoors $ r :
        makeGenome 
            (insertRoom M.empty r)
            r cs [] []

    makeGenome :: Dungeon -> Room -> [DungeonTree] -> [DungeonTree] -> [Room] -> [Room]
    makeGenome d parent ((Leaf r):cs) next rooms = 
        case insertChildRoom d parent r of
            Nothing -> makeGenome d parent cs next rooms
            Just (d2, p2, c2) -> makeGenome d2 p2 cs next (c2:rooms)

    makeGenome d parent ((Node r _ subCs):cs) next rooms =
        case insertChildRoom d parent r of
            Nothing -> makeGenome d parent cs next rooms
            Just (d2, p2, c2) -> makeGenome d2 p2 cs (next ++ [Node c2 0 subCs]) rooms

    makeGenome d parent [] ((Node c _ cs):ns) rooms = makeGenome d c cs ns (parent:rooms)

    makeGenome _ _ [] [] rooms = rooms

    --delete unused doors
    purgeDoors :: [Room] -> [Room]
    purgeDoors [] = []
    purgeDoors (Room ts ds:rs) = 
        Room (S.filter (\t -> notElem t oTiles) ts) ( ds \\ opens) : purgeDoors rs
      where
        opens = filter (\(_,c) -> c == Open) ds
        oTiles = map fst opens

    --take a set of rooms and create a Dungeon
    genomeToDungeon :: [Room] -> Dungeon
    genomeToDungeon =
        foldl insertRoom (generateDungeonGrid 0 0)

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


    byRooms :: DungeonTree -> Int
    byRooms = length . treeToGenome

    targetSize :: Int -> DungeonTree -> Int
    targetSize max t
        | l < max   = l
        | otherwise = 0
      where
        l = length $ treeToGenome t 

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

    baseSeg :: Dungeon
    baseSeg = M.fromList [((x,y),Floor) | x <- [0..3], y<- [0..3]]

    addWallsToSeg :: Room -> CoOrd -> Dungeon
    addWallsToSeg (Room tiles _) at =
        M.mapKeys (\c -> (.+) (at .* 4) c)
        $ foldl' (\s c -> M.insert c Wall s) baseSeg toAdd
      where
        toAdd = concat 
            $ map (side . fst) 
            $ filter (\(_,c) -> S.notMember (at .+ c) tiles)
            $ zip [1..] $ [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

    addEmbiggenedRoom :: Dungeon -> Room -> Dungeon
    addEmbiggenedRoom dg r@(Room tiles _) =
        foldl' (\d s -> M.union s d) dg
        $ map (addWallsToSeg r) 
        $ S.toList tiles

    unsealDoor :: Dungeon -> CoOrd -> Dungeon
    unsealDoor small at =
        M.mapKeys (\c -> (.+) (at .* 4) c)
        $ foldl' (\s c -> M.insert c Wall s) baseSeg toAdd
      where
        toAdd = concat 
            $ map (side . fst) 
            $ filter (\(_,c) -> M.findWithDefault Empty (c .+ at) small == Empty)
            $ zip [1..] $ [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

    unsealDoors :: Dungeon -> Dungeon -> Dungeon
    unsealDoors sealed small =
        foldl' (\d s -> M.union s d) sealed
        $ map (unsealDoor small) doors
      where
        doors   = M.keys $ M.filter (== Conn) small
        
    embiggenDungeon' :: [Room] -> Dungeon
    embiggenDungeon' genome =
        unsealDoors sealed dungeon
      where
        dungeon = genomeToDungeon genome
        sealed  = foldl' (\d r -> addEmbiggenedRoom d r) M.empty genome


    embiggenDungeon :: DungeonTree -> Dungeon
    embiggenDungeon tree =
        unsealDoors sealed dungeon
      where
        genome  = treeToGenome tree
        dungeon = genomeToDungeon genome
        sealed  = foldl' (\d r -> addEmbiggenedRoom d r) M.empty genome


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
    printDungeonPixel Conn = doorPixel
    printDungeonPixel Occupied = tilePixel
    printDungeonPixel Empty = PixelRGB8 255 255 255
    printDungeonPixel Wall = tilePixel
    printDungeonPixel Floor = PixelRGB8 100 100 100

    test = do
        rooms <- allRooms
        s     <- newPureMT
        let crooms    = choiceFromList $ zip [1,1..] rooms 
            (pop1,s1) = randomTrees 400 crooms 100 6 s
            (x,   sx) = geneticDungeon 10 (targetSize 100) pop1 rooms s1
        printRawDungeon "dungeonRawOut.png" $ embiggenDungeon x
