{-# LANGUAGE RecordWildCards #-}
module DemiGen.TileGen where
    import          DemiGen.Types

    import           Data.Maybe

    import           Data.List
    import qualified Data.Map as M
    import qualified Data.Heap as H
    import qualified Data.Set as S
    import qualified Data.Heap as H
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
        { utiles       :: [TileImg]
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
        | x <- [0,n.. imageWidth img - 1]
        , y <- [0,n.. imageHeight img - 1]
        ]

    --get frequency of appearances by unique tiles
    getTileFrequencies :: [TileImg] -> [TileImg] -> TileFreqs
    getTileFrequencies unique tiles =
        foldl' (\m t -> M.insertWith (+) (fromJust $ elemIndex t unique) 1 m) M.empty tiles

    --find neighbours to the target tile in the target direction
    getNeighboursAt :: TileImg -> Int -> [TileImg] -> TileImg -> CoOrd -> Set Int
    getNeighboursAt img n unique target dir =
        S.delete (-1)
        $ foldl' (\s c -> S.insert (pickNeighbour dir c) s) S.empty
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

    getSubset ::  Int -> [Transform] -> [TileImg] -> TileImg -> Set Int
    getSubset n [] uniques section = getSubset n [noTransform] uniques section

    getSubset n transforms uniques section =
        S.fromList 
        $ map (\t -> fromJust $ elemIndex t uniques) 
        $ nub . concat $ map (map fst . (getTiles n))
        $ [t section | t <- transforms]


--wave initialising functions
---------------------------------------------------------------------------------------------------

    generateStartingWave :: [CoOrd] -> Set Int -> Wave
    generateStartingWave shape freqs = foldl' (\m c -> M.insert c freqs m) M.empty shape


--core wfc functions
---------------------------------------------------------------------------------------------------

    selectNextCoOrd :: WaveFunction -> PureMT -> Either PureMT (CoOrd, WaveFunction)
    selectNextCoOrd wf@WaveFunction{..} s
        | H.null heap      = Left s
        | M.member c input = Right (c, drop1H wf)
        | otherwise        = selectNextCoOrd (drop1H wf) s
      where
        c = snd . head $ H.take 1 heap

    observePixel :: Rules -> WaveFunction -> PureMT -> CoOrd -> Either PureMT (WaveFunction, PureMT)
    observePixel Rules{..} WaveFunction{..} s at 
        | length possible == 0 = Left s
        | otherwise            = Right (observedWF, s2)
      where
        possible       = input M.! at
        weighted       = foldl' (\w p -> (p,frequencies M.! p) : w) [] possible
        (selected, s2) = randomFrom weighted s
        observedWF     = WaveFunction (M.insert at (S.singleton selected) input) (M.insert at selected output) heap
 
    getNeighbours :: WaveFunction -> CoOrd -> Set (CoOrd, CoOrd)
    getNeighbours WaveFunction{..} at = S.fromList
        [ (n,d) 
        | d <- dirs
        , let n = at .+ d
        , M.member n input, M.notMember n output
        ]
    
    collapsePixel :: Rules -> (WaveFunction, Set CoOrd) -> CoOrd -> CoOrd -> Set Int -> (WaveFunction, Set CoOrd)
    collapsePixel Rules{..} (wf@WaveFunction{..}, updated) at from enablers
        | target == possible = (wf, updated)
        | otherwise          = (collapsed, S.insert at updated)
      where
        target    = input M.! at
        valid     = S.foldl' (\s ti -> S.union s $ adjacency M.! (ti, from)) S.empty enablers
        possible  = S.intersection target valid
        collapsed = WaveFunction (M.insert at possible input) output heap

    collapseNeighbours :: Rules -> (WaveFunction, Set CoOrd) -> CoOrd -> (WaveFunction, Set CoOrd)
    collapseNeighbours rules (wf@WaveFunction{..},updated) from =
        S.foldl' (\w (at, d) -> collapsePixel rules w at d enablers) (wf,updated) $ getNeighbours wf from
      where
          enablers = input M.! from

    getEntropy :: TileFreqs -> Set Int -> Double
    getEntropy weights valid = sum [dw * logBase 2 dw | v <- S.toList valid, let dw = fromIntegral $ weights M.! v]

    propagate :: Rules -> WaveFunction -> Set CoOrd -> Set CoOrd -> WaveFunction
    propagate rules@Rules{..} wf@WaveFunction{..} todo updated
        | S.null todo = settleWave rules wf updated
        | otherwise   = propagate rules collapsed newUpdated $ S.union updated newUpdated
      where
        (collapsed, newUpdated) = S.foldl' (\w n -> collapseNeighbours rules w n) (wf, S.empty) todo

    --get the entrophies of all updated tiles
    settleWave :: Rules -> WaveFunction -> Set CoOrd -> WaveFunction
    settleWave Rules{..} WaveFunction{..} updated =
        WaveFunction input output
        $ foldl'   (\h (u,e) -> H.insert (u,e) h)               heap 
        $ map      (\(u, vs) -> (getEntropy frequencies vs, u)) 
        $ S.foldl' (\l u     -> (u, input M.! u) : l)           [] updated

    deleteObserved :: WaveFunction -> CoOrd -> WaveFunction
    deleteObserved WaveFunction{..} at = WaveFunction (M.delete at input) output heap

    collapseWave :: Rules -> WaveFunction -> PureMT -> Either PureMT Grid
    collapseWave rules@Rules{..} wf s
        | (M.size $ input wf) == 0 = Right $ output wf
        | otherwise               = do
            (next, wf1) <- trace (show $ M.size $ input wf) selectNextCoOrd wf s
            (wf2,s2)    <- observePixel rules wf1 s next
            let settledWave = propagate rules wf2 (S.singleton next) S.empty
            collapseWave rules (deleteObserved settledWave next) s2

--misc in/out functions
---------------------------------------------------------------------------------------------------

    forceTile :: Rules -> WaveFunction -> Int -> CoOrd -> WaveFunction
    forceTile rules wf@WaveFunction{..} ti at =
        deleteObserved (propagate rules added (S.singleton at) S.empty) at
      where
        added = WaveFunction (M.insert at (S.singleton ti) input) (M.insert at ti output) heap




    generateUntilValid :: Rules -> WaveFunction -> PureMT -> Grid
    generateUntilValid r w s =
        case collapseWave r w s of
            Left sn -> trace ("failed") $ generateUntilValid r w sn
            Right g -> g

    defaultTile :: Int -> TileImg
    defaultTile n = generateImage defaultTilePixel n n

    makeImage :: Rules -> Grid -> Int -> TileImg
    makeImage Rules{..} collapsed n =
        below [generateRow minx maxx yy | yy <- [miny..maxy]]
      where
        (minx,miny,maxx,maxy) = getBounds (M.keys collapsed) (-5,-5,5,5)
        generateRow minx maxx y = beside 
            [ case collapsed M.!? (xx,y) of
                Just r -> utiles !! r
                Nothing -> defaultTile n 
            | xx <- [minx..maxx]
            ]

