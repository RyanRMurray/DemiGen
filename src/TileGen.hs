module TileGen where
    import Text.XML as XML
    import Text.XML.Lens

    data SymType = X | T | I | L | O deriving (Show)    
    data Tile = Tile 
        { tileName  :: String
        , symmetry  :: SymType
        , weighting :: Rational
        } deriving (Show)

    
--functions for parsing XML for tiles
-----------------------------------------------------------------------------------------------------------------

--parseXML :: String -> Either ? [String]
--parseXML fpath = do
  --  Document _ root _ <- readFile def fpath
    
