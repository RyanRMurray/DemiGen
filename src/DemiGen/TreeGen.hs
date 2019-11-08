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

    type Door = Door
        { pos  :: CoOrd
        , dir  :: CoOrd
        , conn :: Maybe Room
        }

    data Cell = Empty | Room | Door

    type Room = Room
        { tiles :: Set CoOrd
        , doors :: Set Door
        }

--Functions for generating input room templates
--------------------------------------------------------------------------------------------------------

    