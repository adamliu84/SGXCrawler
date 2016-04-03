import Network.HTTP.Conduit
import Network.HTTP
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr)
import Data.List (isInfixOf)
import Data.String.Utils (split, replace)
import qualified Data.ByteString as B

--http://www.sgx.com/wps/portal/sgxweb/home/company_disclosure/isin_code_download
url = "http://infopub.sgx.com/FileOpen/.ashx?App=ISINCode&FileID=1"

downloadPage ::  IO [String]
downloadPage = do 
                  return.lines.bsToStr =<< simpleHttp url
		  where bsToStr = map (chr . fromEnum).LB.unpack

main = do
	lSym <- downloadPage
	mapM_ print lSym
	print "SDF"
