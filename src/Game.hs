module Game ()

where
-- import Control.Zipper
import Data.Vector ( MVector )

newtype Lattice = MVector Tac

type Tac = Maybe Move

data Move = Move { player :: Player
                 , strategy :: Strategy
                 }

data Strategy = Solicit

data Player = Player { name :: String
                     , mark :: Char
                     }
