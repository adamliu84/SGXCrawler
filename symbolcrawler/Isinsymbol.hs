import Network.HTTP.Conduit
import Network.HTTP
import qualified Data.ByteString.Lazy as LB (unpack)
import Data.Char (chr)
import Data.String.Utils (replace)
import Data.List
import qualified Data.Text as T (pack, unpack,strip)
import qualified Utility (initFile)

csvfilename = "..//SGXSymbol.csv"
--http://www.sgx.com/wps/portal/sgxweb/home/company_disclosure/isin_code_download
url = "http://infopub.sgx.com/FileOpen/.ashx?App=ISINCode&FileID=1"

data Company = Company { name :: String
            ,status :: String
            ,iSINCode :: String
            ,code :: String
            ,shortName :: String            
            } deriving (Show)  

instance Eq Company where
      c1 == c2 = (code c1) == (code c2)

instance Ord Company where
      c1 `compare` c2 = (code c1) `compare` (code c2)

downloadPage ::  IO [String]
downloadPage = do 
                  return.lines.bsToStr =<< simpleHttp url
          where bsToStr = map (chr . fromEnum).LB.unpack
                        
toCompany :: String -> Company
toCompany x = let  
        nameT = splitAt 50 x           
        statusT = splitAt 2 (snd nameT)
        isinT = splitAt 20 (snd statusT)
        codeT = splitAt 12 (snd isinT)
        shortName = usp $ replace "\r" "" (snd codeT)
        in Company (retFst nameT) (retFst statusT) (retFst isinT) (retFst codeT) (shortName)
        where retFst = usp.fst
              usp = T.unpack.T.strip.T.pack
        
companyHeaderRow :: Company
companyHeaderRow = Company "Name" "Status" "iSInCode" "Code" "ShortName"
writeCSVData :: Company -> IO ()
writeCSVData coy = appendFile csvfilename $ ((code coy)++";"++(name coy)) ++ "\n"

main = do
    -- Parse sgx page listing
    temp <- downloadPage
    let lSym = sort $ map toCompany temp

    -- Write csv file
    Utility.initFile csvfilename >> mapM_ writeCSVData (companyHeaderRow:tail lSym)

    print "FIN"

