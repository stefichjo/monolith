module FileSystem where

import System.IO (IOMode(..), hGetContents, hPrint, openFile, withFile)

getFile :: Read a => FilePath -> IO a
getFile = (read <$>) . readFileContents

putFile :: Show a => FilePath -> a -> IO ()
putFile = writeFileContents WriteMode

addFile :: Show a => FilePath -> a -> IO ()
addFile = writeFileContents AppendMode

initFile :: FilePath -> IO ()
initFile file = putFile file ""

directory :: FilePath
directory = "out/"

readFileContents :: FilePath -> IO String
readFileContents file = openFile (directory ++ file) ReadMode >>= hGetContents

writeFileContents :: Show a => IOMode -> FilePath -> a -> IO ()
writeFileContents mode file = withFile (directory ++ file) mode . flip hPrint
