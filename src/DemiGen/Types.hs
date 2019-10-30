module DemiGen.Types where

    import           Codec.Picture
    import           Codec.Picture.Extra

    import           Data.Map (Map)
    import           Data.Heap (MinHeap)
    import           Data.Set (Set)

    type Pix = PixelRGB8
    type TileImg = Image Pix                        --Type of image to import and export
   
    type TileFreqs = Set (Int, Rational)            --Set of tile indexes with their relative rarity
    type CoOrd = (Int, Int)                         --Simple cartesian integer coordinates
    type Wave = Map CoOrd TileFreqs                 --Un or partially collapsed wavefunction containing a grid of possible tile indexes
    type Collapsed = Map CoOrd Int                  --Fully collapsed wavefunction containing a grid of tile indexes
    type Neighbor = (CoOrd,CoOrd)                   --Coordinates of a neighboring tile and its relative direction
    type EntropyHeap = MinHeap (Rational, CoOrd)    --Heap listing partially collapsed cells by their relative entropy
    type EnabledTiles = Map (Int, CoOrd) (Set Int)  --Map of tiles and a direction onto what tiles are valid for that space
    
    type Transform = TileImg -> TileImg             --Transformations for enriching input tile samples

    defaultTilePixel = PixelRGB8 255 0 127


--Simple functions for grid-based processes

    dirs :: [CoOrd]
    dirs = [(0,-1),(-1,0),(0,1),(1,0)]

    getGrid :: Int -> Int -> [CoOrd]
    getGrid x y = concat [getGrid' x row| row <- [0..y]]
    getGrid' x row = [(col,row) | col <- [0..x]]

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