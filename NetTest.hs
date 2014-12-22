import Network.Socket hiding (recv)
import System.IO
import Control.Concurrent
import IrcMessage as M

main :: IO ()
main = do
    a <- address "irc.dal.net" 7000
    putStrLn $ show a
    s <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
    connect s (addrAddress a)
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering
    rchan <- newChan
    forkIO (reader h rchan)
    m <- readChan rchan
    putStrLn $ show m
    hPutStr h "NICK dbanerjee1979\r\n" 
    hPutStr h "USER guest localhost irc.dal.net :Joe\r\n" 
    readLoop rchan

readLoop rchan = do
    m <- readChan rchan  
    putStrLn $ show m
    readLoop rchan

reader :: Handle -> Chan Message -> IO ()
reader h rchan = do
    loop h rchan
    hClose h

loop h rchan = do
    l <- hGetLine h
    case M.parse l of
        Nothing -> putStrLn $ "Unable to parse response " ++ l
        Just m -> writeChan rchan m
    loop h rchan

address :: String -> Int -> IO AddrInfo
address hostname port = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    return $ head addrs
