import Network.HTTP.Conduit
import Network.HTTP
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr)
import Data.String.Utils (split)

csvfilename = "fulllisting.csv"

--http://www.sgx.com/wps/portal/sgxweb/home/company_disclosure/isin_code_download
url = "http://infopub.sgx.com/FileOpen/.ashx?App=ISINCode&FileID=1"


downloadPage ::  IO [String]
downloadPage = do 
                  return.lines.bsToStr =<< simpleHttp url
		  where bsToStr = map (chr . fromEnum).LB.unpack

writeCSVHeader = appendFile csvfilename "Name;Status;ISINCode;Code;ShortName"
writeCSVData x = appendFile csvfilename x

main = do
	-- Parse sgx page listing
	lSym <- downloadPage

	-- Write csv file
	writeCSVHeader
	mapM_ writeCSVData lSym

	print "FIN"
