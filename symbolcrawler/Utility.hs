module Utility (
initFile
) where

import Data.Time
import Data.Time.Format
import System.IO.Unsafe
import Data.List.Split
import Data.String.Utils (replace)

initFile :: String -> IO ()
initFile szFilename = writeFile szFilename ""

getCurrentDateTimeStamp :: IO String
getCurrentDateTimeStamp = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ concat[yyyymmdd now,"_",(hhmm now timezone)]
    where yyyymmdd n = let (a,b,c) = toGregorian.utctDay $ n
                           result = concat [show a, pad2zero b, pad2zero c]
                       in result
          hhmm n t = let a = localTimeOfDay $ utcToLocalTime t n                         
                   in concat[pad2zero (todHour a), pad2zero (todMin a)]
          pad2zero x = if x < 10 then
                    reverse.show $ x * 10
                else
                    show x                       