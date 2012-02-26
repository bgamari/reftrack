import Data.Maybe (fromJust)
import Network.URI (parseURI)
import OAI

arxiv_oai_url = fromJust $ parseURI "http://export.arxiv.org/oai2"

--main = oaiRequest arxiv_oai_url [("verb", "GetRecord"), ("identifier", "oai:arXiv.org:quant-ph/9901001"), ("metadataPrefix","oai_dc")] >>= print
main = do a <- runX (getRecord arxiv_oai_url "oai:arXiv.org:quant-ph/9901001" >>> parseDCRecord)
          print a


