import RefTrack.Types
import Data.Acid.Local
import Data.Acid.Remote
import Network

portId = PortNumber 7777

main = do
    state <- openLocalState emptyRepo
    acidServer state portId       
