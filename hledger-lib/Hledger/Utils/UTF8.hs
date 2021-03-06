{-
From pandoc, slightly extended. Example usage:

 import Prelude hiding (readFile,writeFile,getContents,putStr,putStrLn)
 import Hledger.Utils.UTF8 (readFile,writeFile,getContents,putStr,putStrLn)


----------------------------------------------------------------------
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.UTF8
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

UTF-8 aware string IO functions that will work with GHC 6.10 or 6.12.
-}
module Hledger.Utils.UTF8 ( readFile
                         , writeFile
                         , appendFile
                         , getContents
                         , hGetContents
                         , putStr
                         , putStrLn
                         , hPutStr
                         , hPutStrLn
                         )

where
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Prelude hiding (readFile, writeFile, appendFile, getContents, putStr, putStrLn)
import System.IO (Handle)
import Control.Monad (liftM)

bom :: B.ByteString
bom = B.pack [0xEF, 0xBB, 0xBF]

stripBOM :: B.ByteString -> B.ByteString
stripBOM s | bom `B.isPrefixOf` s = B.drop 3 s
stripBOM s = s

readFile :: FilePath -> IO String
readFile = liftM (toString . stripBOM) . B.readFile

writeFile :: FilePath -> String -> IO ()
writeFile f = B.writeFile f . fromString

appendFile :: FilePath -> String -> IO ()
appendFile f = B.appendFile f . fromString

getContents :: IO String
getContents = liftM (toString . stripBOM) B.getContents

hGetContents :: Handle -> IO String
hGetContents h = liftM (toString . stripBOM) (B.hGetContents h)

putStr :: String -> IO ()
putStr = B.putStr . fromString

putStrLn :: String -> IO ()
putStrLn = B.putStrLn . fromString

hPutStr :: Handle -> String -> IO ()
hPutStr h = B.hPutStr h . fromString

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h (s ++ "\n")
