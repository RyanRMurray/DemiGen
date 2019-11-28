module DemiGen.TreeGen where
    import          DemiGen.Types

    import           Data.List as L
    import qualified Data.Map as M
    import qualified Data.Heap as H
    import qualified Data.Set as S
    import           Data.Map (Map)
    import           Data.Heap (MinHeap)
    import           Data.Set (Set)
    import           Data.Either

    import           System.Random
    import           Control.Monad.Random as Rand

    import           Codec.Picture
    import           Codec.Picture.Extra

--Functions for generating input room templates and starting dungeon
--------------------------------------------------------------------------------------------------------

    getRoomData :: TileImg -> Room
    getRoomData input = Room
        (S.fromList $ getTargetPixels input tilePixel) $ 
        zip 
            doors
            (replicate (length doors) Open)
        where
            doors = getTargetPixels input doorPixel

    roomNames :: [String]
    roomNames =
        [ "hall01", "hall02", "hall03"
        , "room01", "room02", "room03", "room04", "room05", "room06", "room07", "room08", "room09", "room10"
        , "special01", "special02", "special03"
        ]

    allRooms :: IO [Room]
    allRooms = do
            inputs   <-  mapM (\n-> readPng $ "assets/rooms/"++n++".png") roomNames
            return $ map getRoomData $ L.map convertRGB8 $ rights $ inputs

    getTargetPixels :: TileImg -> PixelRGB8 -> [CoOrd]
    getTargetPixels input px = filter
        (\(x,y)-> px == pixelAt input x y) $
        getGrid 12 12

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
    
    offsetRoom :: Room -> CoOrd -> Room
    offsetRoom r (ox,oy) = Room
            (S.map (\(x,y)->(x+ox,y+oy)) $ tiles r) $
            L.map (\((x,y),c)->((x+ox,y+oy),c)) $ doors r

    noCollision :: Dungeon -> Room -> CoOrd -> Bool
    noCollision d (Room ts _) (ox,oy) = 
        and [isFree d (x+ox,y+oy) | (x,y) <- S.toList ts]
    
    isFree d c = or
        [ M.notMember c d
        , d M.! c /= Occupied
        ]

    findValidDoor :: Dungeon -> Room -> Maybe CoOrd
    findValidDoor d (Room _ doors)
        | length valid /= 0 = Just $ fst $ head valid
        | otherwise         = Nothing
      where
        valid = L.filter (\d -> (snd d) == Open) doors
        
    insertRoom :: Dungeon -> Room -> Dungeon
    insertRoom d r =
        L.foldl (\dg c -> M.insert c Conn dg) d2 $ L.map fst $ doors r
        where
            d2 = S.foldl (\dg c -> M.insert c Occupied dg) d (tiles r)

    insertChildRoom :: Dungeon -> Room -> Room -> Maybe (Dungeon, Room, Room)
    insertChildRoom d parent child = do
        target <- findValidDoor d parent
        let rotations     = [rotateRoom child rot | rot <- [0..3]]
            dooroffsets   = [offsetRoom (setConn r parent (x,y)) (-x,-y) | r <- rotations, ((x,y),_) <- doors r]
            offsets       = [offsetRoom r target | r <- dooroffsets]
        (newD, newChild) <- msum [attemptAttach d r | r <- offsets]        
        Just (newD, (setConn parent newChild target), newChild)

    attemptAttach :: Dungeon -> Room -> Maybe (Dungeon, Room)
    attemptAttach d r 
        | noCollision d r (0,0) = Just $ (insertRoom d r, r)
        | otherwise             = Nothing

    setConn :: Room -> Room -> CoOrd -> Room
    setConn (Room ts doors) connected door =
        Room ts $ (door, To connected) : [(c, t) | (c,t) <- doors, c /= door]
          

--random tree generator
---------------------------------------------------------------------------------------------------

    randomTree :: [Room] -> Int -> Int -> StdGen -> (DungeonTree, StdGen)
    randomTree rooms 0 _ seed = Rand.runRand ( Rand.fromList [(Leaf r,1.0) | r <- rooms] ) seed
    randomTree rooms budget maxChildren seed =
        let (childNum, seed2) = randomR (1,maxChildren) seed:: (Int, StdGen)
            (cBudgets, seed3) = distributeBudget (budget-1) childNum ([], seed2)
            (children, seed4) = randomChildren rooms cBudgets maxChildren ([], seed3)
            (pRoom, seed5)    = Rand.runRand ( Rand.fromList [(r,1.0)| r <- rooms] ) seed
        in
            (Node pRoom budget children, seed5)


    distributeBudget :: Int -> Int -> ([Int], StdGen) -> ([Int], StdGen)
    distributeBudget 0 _ result = result
    distributeBudget budget 0 (res, seed) = (budget:res, seed)
    distributeBudget budget recipients (distributed, seed) =
        let (newDis, seed2) = randomR (0, budget) seed
            remaining       = budget - newDis
        in
            distributeBudget remaining (recipients - 1) (newDis:distributed, seed2)

    randomChildren :: [Room] -> [Int] -> Int -> ([DungeonTree], StdGen) -> ([DungeonTree], StdGen)
    randomChildren _ [] _ result = result
    randomChildren rooms (c:cs) maxChildren (res, seed) =
        randomChildren rooms cs maxChildren (newC:res, seed2)
      where
        (newC, seed2) = randomTree rooms c maxChildren seed

--tree mutation functions
---------------------------------------------------------------------------------------------------

    mutate :: [Room] -> DungeonTree -> DungeonTree -> StdGen -> (DungeonTree, StdGen)
    mutate rooms donor recipient s
        | length muts4 == 0 = change rooms donor recipient s
        | otherwise        = foldl (\(r,s) f -> f rooms donor r s) (recipient,s5) muts4
      where
        (muts,  s2) = Rand.runRand (Rand.fromList [([crossover], 0.7), ([], 0.3)]) s
        (muts2, s3) = Rand.runRand (Rand.fromList [(muts ++ replicate 3 grow, 0.5), (muts, 0.5)]) s
        (muts3, s4) = Rand.runRand (Rand.fromList [(muts2 ++ [trim], 0.5), (muts2, 0.5)]) s
        (muts4, s5) = Rand.runRand (Rand.fromList [(muts3 ++ replicate 2 change, 0.5), (muts3, 0.5)]) s

    crossover :: [Room] -> DungeonTree -> DungeonTree -> StdGen -> (DungeonTree, StdGen)
    crossover _ donor recipient s =
        applyToRandom f recipient s2
      where
        (new,s2) = getRandomSubTree donor s
        f        = swapSubTree new

    swapSubTree :: DungeonTree -> DungeonTree -> StdGen -> (DungeonTree, StdGen)
    swapSubTree new host s = (new, s)

    grow :: [Room] -> DungeonTree -> DungeonTree -> StdGen -> (DungeonTree, StdGen)
    grow choices _ recipient s =
        applyToRandom f recipient s5
      where
        (choice1, s2) = Rand.runRand (Rand.fromList [(r,1.0)| r<-choices]) s
        (choice2, s3) = Rand.runRand (Rand.fromList [(r,1.0)| r<-choices]) s2
        (choice3, s4) = Rand.runRand (Rand.fromList [(r,1.0)| r<-choices]) s3
        (choice4, s5) = Rand.runRand (Rand.fromList [(r,1.0)| r<-choices]) s4
        chosen        = [Leaf choice1, Leaf choice2, Leaf choice3, Leaf choice4]
        f (Leaf r) sd = (Node r 3 chosen, sd)
        f (Node r size children) sd = (Node r size (chosen ++ children), sd) 

    trim :: [Room] -> DungeonTree -> DungeonTree -> StdGen -> (DungeonTree, StdGen)
    trim _ _ recipient s =
        applyToRandom f recipient s
      where
        f (Leaf _) sd = (Null, sd)
        f node     sd = trim [] Null node sd

    change :: [Room] -> DungeonTree -> DungeonTree -> StdGen -> (DungeonTree, StdGen)
    change choices _ recipient s =
        applyToRandom f recipient s2
      where
        (choice, s2)   = Rand.runRand (Rand.fromList [(r,1.0)| r<-choices]) s
        f (Leaf r)     sd = (Leaf choice, sd)
        f (Node r s c) sd = (Node choice s c, sd)

    getRandomSubTree :: DungeonTree -> StdGen -> (DungeonTree, StdGen)
    getRandomSubTree (Leaf r) s = ((Leaf r), s)
    getRandomSubTree parent s 
        | choice == -1 = (parent, s2)
        | otherwise    = getRandomSubTree (children !! choice) s2
      where
        (Node _ _ children) = parent
        choiceList =  (-1, 1.0) : [(i, getSize c) | (i,c) <- zip [0..length children] children]
        (choice, s2)     = Rand.runRand (Rand.fromList choiceList) s

    applyToRandom :: (DungeonTree -> StdGen -> (DungeonTree, StdGen)) -> DungeonTree -> StdGen -> (DungeonTree, StdGen)
    applyToRandom f (Leaf r) s = f (Leaf r) s
    applyToRandom f parent s 
        | choice == -1 = f parent s2
        | otherwise    = applyToRandom' f parent choice s2
      where
        (Node _ _ children) = parent
        choiceList          = (-1, 1.0) : [(i, getSize c) | (i,c) <- zip [0..length children] children]
        (choice, s2)        = Rand.runRand (Rand.fromList choiceList) s
        
    applyToRandom' f (Node r size children) choice s 
        | updatedChild == Null = (Node r (size-1) children, s2)
        | otherwise            = (Node r size (updatedChild:otherChildren), s2)
      where
        (updatedChild, s2) = applyToRandom f (children !! choice) s
        otherChildren      = dropN children choice

    dropN :: [a] -> Int -> [a]
    dropN [] _ = []
    dropN (x:xs) n 
     | n == 0    = xs
     | otherwise = x : dropN xs (n-1) 

        
    getSize :: DungeonTree -> Rational
    getSize (Leaf _) = 1
    getSize (Node _ size _) = fromIntegral size

--tree to dungeon functions
---------------------------------------------------------------------------------------------------

    treeToGenome :: DungeonTree -> [Room]
    treeToGenome (Leaf (Room ts _)) = [Room ts []]

    treeToGenome (Node r _ cs) = purgeDoors $
        makeGenome 
            (insertRoom (generateDungeonGrid 0 0) r) 
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

    purgeDoors :: [Room] -> [Room]
    purgeDoors [] = []
    purgeDoors ((Room ts ds):rs) = 
        (Room ts [d | d <- ds, snd d /= Open]) : purgeDoors rs


    genomeToDungeon :: [Room] -> Dungeon
    genomeToDungeon =
        foldl insertRoom (generateDungeonGrid 0 0)


--test outputs
---------------------------------------------------------------------------------------------------
    
    printBoundedRawDungeon :: Dungeon -> Int -> Int -> IO ()
    printBoundedRawDungeon d x y = writePng "dungeonRawOut.png" $
        generateImage (\x y -> printDungeonPixel $ M.findWithDefault Empty (x,y) d) x y

    printRawDungeon :: Dungeon -> IO ()
    printRawDungeon d =
        let (minx,miny,maxx,maxy)  = getBounds (M.keys d) (0,0,0,0)
            newD                   = M.mapKeys (\(x,y) -> (x + abs minx,y + abs miny)) d
        in
            writePng "dungeonRawOut.png" $ 
            generateImage 
                (\x y -> printDungeonPixel $ M.findWithDefault Empty (x+minx,y+miny) newD) 
                (maxx + abs minx + 50) (maxy + abs miny + 50)

    getBounds :: [CoOrd] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
    getBounds [] res = res
    getBounds ((x,y):cs) (minx,miny,maxx,maxy) =
        getBounds cs (nminx, nminy, nmaxx, nmaxy)
      where
        nminx = min x minx
        nmaxx = max x maxx
        nminy = min y miny
        nmaxy = max y maxy


    printDungeonPixel :: Cell -> PixelRGB8
    printDungeonPixel Conn = doorPixel
    printDungeonPixel Occupied = tilePixel
    printDungeonPixel Empty = PixelRGB8 255 255 255
