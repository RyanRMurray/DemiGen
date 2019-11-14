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
        zip doors
            (replicate (length doors) Open)
        where
            doors = (getTargetPixels input doorPixel)

    roomNames :: [String]
    roomNames =
        [ "hall01", "hall02", "hall03"
        , "room01", "room02", "room03", "room04", "room05", "room06", "room07", "room08", "room09", "room10"
        , "special01", "special02", "special03"
        ]

    allRooms :: IO [Room]
    allRooms = do
            inputs   <-  mapM (\n-> readPng $ "assets/rooms/"++n++".png") roomNames
            let rdata = map getRoomData $ L.map convertRGB8 $ rights $ inputs
                x     = [rotateRoom room rot | room <- rdata, rot <- [0..3]]
            return x

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
        L.map (\(c,n) -> (f c,n)) $ doors r
    
    offsetRoom :: Room -> CoOrd -> Room
    offsetRoom r (ox,oy) = Room
            (S.map (\(x,y)->(x+ox,y+oy)) $ tiles r) $
            L.map (\((x,y),n)->((x+ox,y+oy),n)) $ doors r

    noCollision :: Dungeon -> Room -> CoOrd -> Bool
    noCollision d (Room ts _) (ox,oy) = 
        and [isFree d (x+ox,y+oy) | (x,y) <- S.toList ts]
    
    isFree d c = d M.! c /= Occupied



    findValidDoor :: Dungeon -> Room -> Maybe CoOrd
    findValidDoor d (Room _ doors) = findValidDoor' d doors
    findValidDoor' d [] = Nothing
    findValidDoor' d ((at,c):doors)
        | c == Open = Just at
        | otherwise = findValidDoor' d doors


    attachRoomToDoor :: Dungeon -> Room -> CoOrd -> Maybe Dungeon
    attachRoomToDoor d r target 
        | noCollision d r target = Just $ insertRoom d r target
        | otherwise              = Nothing

    insertRoom :: Dungeon -> Room -> CoOrd -> Dungeon
    insertRoom d r (ox, oy) =
        L.foldl (\dg ((x,y),_) -> M.insert (x+ox,y+oy) Conn dg) d2 $ doors r
        where
            d2 = S.foldl (\dg (x,y) -> M.insert (x+ox,y+oy) Occupied dg) d (tiles r)



--test outputs
---------------------------------------------------------------------------------------------------
    
    printRawDungeon :: Dungeon -> Int -> Int -> IO ()
    printRawDungeon d x y = writePng "dungeontestout.png" $
        generateImage (\x y -> printDungeonPixel $ d M.! (x,y)) x y

    printDungeonPixel :: Cell -> PixelRGB8
    printDungeonPixel Conn = doorPixel
    printDungeonPixel Occupied = tilePixel
    printDungeonPixel Empty = PixelRGB8 255 255 255
