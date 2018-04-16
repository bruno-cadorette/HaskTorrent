module File(File(..)) where

import Control.Concurrent.ReadWriteVar
import Data.List
import System.IO
import qualified Data.ByteString as B
import Control.Concurrent.Chan

data BlockStatus = BlockEmpty | BlockFull deriving (Eq, Show)
type BlockId = Integer
data File = File {fileHandles :: Chan Handle, blocks :: ([(RWVar BlockStatus)]),  blockLength :: Integer }

safeHandle file blockNumber f = do 
    handle <- readChan (fileHandles file)
    hSeek handle AbsoluteSeek (blockNumber * (blockLength file))
    ret <- f handle
    writeChan (fileHandles file) handle
    return ret

readBlock file blockNumber = with (head $ blocks file) readFile
    where
        readFile BlockEmpty = pure Nothing
        readFile BlockFull = do
            block <- safeHandle file blockNumber (\handle -> B.hGet handle $ fromInteger (blockLength file))
            pure $ Just block

writeBlock file blockNumber content = do
    modify_ (head (blocks file)) write
    where 
        write BlockFull = pure BlockFull
        write BlockEmpty = do
            safeHandle file blockNumber $ \handle -> B.hPut handle content
            pure BlockFull
--newFile path = fmap (\h -> File h ([]) 16) $ openBinaryFile path ReadWriteMode