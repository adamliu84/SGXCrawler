import Network.HTTP.Conduit
import Network.HTTP
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr)
import Data.String.Utils (split, replace)
import Data.List (intersperse)
import qualified Data.Text as T

csvfilename = "fulllisting.csv"

--http://www.sgx.com/wps/portal/sgxweb/home/company_disclosure/isin_code_download
url = "http://infopub.sgx.com/FileOpen/.ashx?App=ISINCode&FileID=1"

data Company = Company { name :: String
			,status :: String
			,iSINCode :: String
			,code :: String
			,shortName :: String			
			} deriving (Show)  

--toCompany x = Company (getName x) (getStatus x) (getIsin x) (getCode x) (getShortName x)
toCompany x = let  
		nameT = splitAt 50 x           
		statusT = splitAt 2 (snd nameT)
		isinT = splitAt 20 (snd statusT)
		codeT = splitAt 12 (snd isinT)
		shortName =  usp $ replace "\r" "" (snd codeT)
	      in Company (retFst nameT) (retFst statusT) (retFst isinT) (retFst codeT) (shortName)
	      where retFst = usp.fst
 		    usp = T.unpack.T.strip.T.pack
		
toCSVRow coy = init.tail $ foldl (\x y-> x++";"++y) "" [(name coy),(status coy),(iSINCode coy),(code coy),(shortName coy)]

downloadPage ::  IO [String]
downloadPage = do 
                  return.lines.bsToStr =<< simpleHttp url
		  where bsToStr = map (chr . fromEnum).LB.unpack

writeCSVHeader = appendFile csvfilename "Name;Status;ISINCode;Code;ShortName\n"
writeCSVData x = appendFile csvfilename $ (toCSVRow x) ++ "\n"

main = do
	-- Parse sgx page listing
	temp <- downloadPage
	let lSym = map toCompany temp

	-- Write csv file
	writeCSVHeader
	mapM_ writeCSVData (tail lSym)	

	print "FIN"

