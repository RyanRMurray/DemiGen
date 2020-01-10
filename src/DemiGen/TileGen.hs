{-# LANGUAGE RecordWildCards #-}
module DemiGen.TileGen where
    import          DemiGen.Types

    import           Data.Maybe

    import           Data.List
    import qualified Data.Map as M
    import qualified Data.Heap as H
    import qualified Data.Set as S
    import           Data.Map (Map)
    import           Data.Heap (MinHeap)
    import           Data.Set (Set)
    import           Data.Ord
    import Debug.Trace

    import           System.Random.Mersenne.Pure64
    import           System.Random.Shuffle

    import           Codec.Picture
    import           Codec.Picture.Extra


    data Rules = Rules
        { tiles       :: [TileImg]
        , frequencies :: TileFreqs
        , adjacency   :: Map (Int, CoOrd) (Set Int)
        }

    data WaveFunction = WaveFunction
        { input  :: Wave
        , output :: Grid 
        , heap   :: EntropyHeap
        }

    drop1H :: WaveFunction -> WaveFunction
    drop1H WaveFunction{..} = WaveFunction input output (H.drop 1 heap)

--Functions for parsing an input image
--------------------------------------------------------------------------------------------------------

    --get all tiles of dimensions n
    getTiles :: Int -> TileImg -> [(TileImg,CoOrd)]
    getTiles n img =
        [ (crop x y n n img,(x,y))
        | x <- [0,n.. imageWidth img]
        , y <- [0,n.. imageHeight img]
        ]

    --get frequency of appearances by unique tiles
    getTileFrequencies :: [TileImg] -> [TileImg] -> TileFreqs
    getTileFrequencies unique tiles =
        S.fromList
        $ M.toList
        $ foldl' (\m t -> M.insertWith (+) (fromJust $ elemIndex t unique) 1 m) M.empty tiles

    --find neighbours to the target tile in the target direction
    getNeighboursAt :: TileImg -> Int -> [TileImg] -> TileImg -> CoOrd -> Set Int
    getNeighboursAt img n unique target dir =
        foldl' (\s c -> S.insert (pickNeighbour dir c) s) S.empty
        $ [c | (t,c) <- getTiles n img, t == target]
      where
        pickNeighbour (0,-1) (x,y) = if (y>0)                 then fromJust $ elemIndex (crop x (y-n) n n img) unique else -1
        pickNeighbour (1, 0) (x,y) = if (x+n<imageWidth img)  then fromJust $ elemIndex (crop (x+n) y n n img) unique else -1
        pickNeighbour (0, 1) (x,y) = if (y+n<imageHeight img) then fromJust $ elemIndex (crop x (y+n) n n img) unique else -1
        pickNeighbour (-1,0) (x,y) = if (x>0)                 then fromJust $ elemIndex (crop (x-n) y n n img) unique else -1
         
    --apply the getNeighboursAt function to all transmutations for all unique tiles in all directions
    getAdjacencyRules :: [TileImg] -> Int -> [TileImg] -> Map (Int, CoOrd) (Set Int)
    getAdjacencyRules imgs n unique = 
        foldl' (\m (i,d,v) -> M.insertWith S.union (i,d) v m) M.empty $
        [ (ti, dir, valid) 
        | ti <- [0..length unique - 1]
        , dir <- dirs
        , img <- imgs
        , let valid = getNeighboursAt img n unique (unique !! ti) dir 
        ]

    parseRules :: TileImg -> Int -> [Transform] -> Rules
    parseRules img n [] = parseRules img n [noTransform]

    parseRules img n transforms =
        Rules unique tFreq adjs
      where
        transmutations = [t img | t <- transforms]
        tiles          = concat $ map (map fst . (getTiles n)) transmutations
        unique         = nub tiles
        tFreq          = getTileFrequencies unique tiles
        adjs           = getAdjacencyRules transmutations n unique

--wave initialising functions
---------------------------------------------------------------------------------------------------

    generateStartingWave :: [CoOrd] -> TileFreqs -> Wave
    generateStartingWave shape freqs = foldl' (\m c -> M.insert c freqs m) M.empty shape


--core wfc functions
---------------------------------------------------------------------------------------------------

    selectNextCoOrd :: WaveFunction -> PureMT -> Either PureMT (CoOrd, WaveFunction)
    selectNextCoOrd wf@WaveFunction{..} s
        | H.size heap == 0 = Left s
        | M.member c input = Right (c, wf)
        | otherwise        = selectNextCoOrd (drop1H wf) s
      where
        c = snd . head $ H.take 1 heap


    collapse :: Rules -> WaveFunction -> PureMT -> Either PureMT Grid
    collapse rules@Rules{..} wf@WaveFunction{..} s
        | M.size input == 0 = Right output
        | otherwise         = do
            (next, wf1) <- selectNextCoOrd wf s
            undefined
