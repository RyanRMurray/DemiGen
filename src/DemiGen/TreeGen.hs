module DemiGen.TreeGen where
    import          DemiGen.Types

    import           Data.List as L
    import qualified Data.Map as M
    import qualified Data.Heap as H
    import qualified Data.Set as S
    import           Data.Map (Map)
    import           Data.Heap (MinHeap)
    import           Data.Set (Set)

    import           System.Random
    import           Control.Monad.Random as Rand

    import           Codec.Picture
    import           Codec.Picture.Extra

    --Think of a room as a node in a tree, where doors are connected nodes.
    data Room = Room
        { tiles :: Set CoOrd
        , doors :: [Door]
        } deriving (Show)

    type Door = (CoOrd, Maybe Room)

    data Cell = Empty | Occupied | Conn
        deriving (Show)

    doorPixel :: PixelRGB8
    doorPixel = PixelRGB8 255 0 0

    tilePixel :: PixelRGB8
    tilePixel = PixelRGB8 0 0 0

--Functions for generating input room templates
--------------------------------------------------------------------------------------------------------

    getRoomData :: TileImg -> Room
    getRoomData input = Room
        (S.fromList $ getTargetPixels input tilePixel) $
        zip doors
            (replicate (length doors) Nothing)
        where
            doors = (getTargetPixels input doorPixel)

    getTargetPixels :: TileImg -> PixelRGB8 -> [CoOrd]
    getTargetPixels input px = filter
        (\(x,y)-> px == pixelAt input x y) $
        getGrid 12 12

    generateDungeonGrid :: Int -> Int -> Map CoOrd Cell
    generateDungeonGrid x y = M.fromList
        [(c, Empty) | c <- getGrid x y]

    rotateRoom :: Room -> Int -> Room
    rotateRoom r 0 = r
    rotateRoom r 1 = rotateRoom' r (\(x,y) -> (y, 12 - x))
    rotateRoom r 2 = rotateRoom' r (\(x,y) -> (12 - x, 12 - y))
    rotateRoom r 3 = rotateRoom' r (\(x,y) -> (12 - y, x))

    rotateRoom' r f = Room
        (S.map f $ tiles r)
        (L.map (\(c,n) -> (f c,n)) $ doors r)
