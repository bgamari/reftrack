import Control.Error
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import RefTrack.OAI.Arxiv

arxiv_oai_url = fromJust $ parseURI "http://export.arxiv.org/oai2"

--main = oaiRequest arxiv_oai_url [("verb", "GetRecord"), ("identifier", "oai:arXiv.org:quant-ph/9901001"), ("metadataPrefix","oai_dc")] >>= print

main = do
    a <- runEitherT $ getRecord arxiv_oai_url "oai:arXiv.org:quant-ph/9901001"
    print a

