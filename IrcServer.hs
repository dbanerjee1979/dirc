module IrcServer
    (
        startServer
    ) where

import Network.Socket hiding (recv)
import System.IO
import Control.Concurrent
import Reactive.Util as R
import IrcMessage as M

startServer :: String -> Int -> EventSource Message -> Chan Message -> IO ()
startServer hostname port esmsg sendChan = do
    a <- address hostname port
    s <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
    connect s (addrAddress a)
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering
    forkIO (reader h esmsg)
    forkIO (writer h sendChan)
    return ()

address :: String -> Int -> IO AddrInfo
address hostname port = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    return $ head addrs

reader :: Handle -> EventSource Message -> IO ()
reader h esmsg = do
    l <- hGetLine h
    case M.parse l of
        Nothing -> putStrLn $ "Unable to parse response " ++ l
        Just m -> fire esmsg m
    reader h esmsg

writer :: Handle -> Chan Message -> IO ()
writer h sendChan = do
    m <- readChan sendChan
    putStr $ M.generate m
    hPutStr h $ M.generate m
    writer h sendChan
