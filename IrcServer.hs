module IrcServer
    (
        startServer
    ) where

import Network.Socket hiding (recv)
import System.IO
import qualified System.IO.UTF8 as U
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
    forkIO (reader h esmsg sendChan)
    forkIO (writer h sendChan)
    return ()

address :: String -> Int -> IO AddrInfo
address hostname port = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    return $ head addrs

reader :: Handle -> EventSource Message -> Chan Message -> IO ()
reader h esmsg sendChan = do
    l <- U.hGetLine h
    case M.parseMsg l of
        Left m  -> putStrLn m
        Right m -> do putStrLn $ show m
                      case m of
                          Ping tgt -> writeChan sendChan (Pong Nothing tgt)
                          _        -> fire esmsg m
    reader h esmsg sendChan

writer :: Handle -> Chan Message -> IO ()
writer h sendChan = do
    m <- readChan sendChan
    putStr $ M.generate m
    hPutStr h $ M.generate m
    writer h sendChan
