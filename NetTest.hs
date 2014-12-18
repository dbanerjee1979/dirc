import Network.Socket hiding (recv)
import System.IO
import Control.Concurrent
import IrcMessage as M

main :: IO ()
main = do
    mvar <- newEmptyMVar
    forkFinally connTest (\_ -> putMVar mvar "Done!") 
    putStrLn "Waiting..."
    msg <- takeMVar mvar
    putStrLn msg

connTest :: IO ()
connTest = do
    a <- address "irc.dal.net" 7000
    putStrLn $ show a
    s <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
    connect s (addrAddress a)
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering
    l <- hGetLine h
    putStrLn $ show $ M.parse l
    hPutStr h "NICK dbanerjee1979\r\n" 
    hPutStr h "USER guest localhost irc.dal.net :Joe\r\n" 
    hFlush h
    l2 <- hGetLine h
    putStrLn $ show $ M.parse l2
    l3 <- hGetLine h
    putStrLn $ show $ M.parse l3
    l4 <- hGetLine h
    putStrLn $ show $ M.parse l4
    l5 <- hGetLine h
    putStrLn $ show $ M.parse l5
    hClose h

address :: String -> Int -> IO AddrInfo
address hostname port = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    return $ head addrs
