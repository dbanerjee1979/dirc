{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as B
import Numeric
import Data.Char
import Data.Word
import Control.Concurrent
import Network.Socket hiding (recv)
import System.IO
import IrcMessage as M
import IrcServer as S
import Reactive.Util as R
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

main :: IO ()
main = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just "chat.freenode.net") (Just $ "6665")
    let addr = head addrs
    s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect s (addrAddress addr)
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering
    forkIO $ readLoop h
    writeLoop h

writeLoop :: Handle -> IO ()
writeLoop h = do
    m <- getLine
    hPutStr h m
    hPutStr h "\r\n"
    writeLoop h

readLoop :: Handle -> IO ()
readLoop h = do
    m <- B.hGetLine h
    putStrLn $ decode (B.head m) (B.tail m)
    readLoop h

decode :: Word8 -> B.ByteString -> [Char]
decode b bs =
    let i = fromIntegral b
        s = if i >= 32 && i <= 127 then [chr i]
                                   else "\\[" ++ (showHex i "") ++ "]"
        l = B.length bs
    in if l == 0 then s else s ++ decode (B.head bs) (B.tail bs)
