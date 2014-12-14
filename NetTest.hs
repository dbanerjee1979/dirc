import Network.Socket hiding (recv)

import System.IO
import IrcMessage

main :: IO ()
main = do
{-
    a <- address "irc.dal.net" 7000
    s <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
    connect s (addrAddress a)
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering
    l <- hGetLine h
    putStrLn $ show l
    hClose h
    msg <- recv s 1024
    putStrLn $ foo
    sClose s
-}
    let s = ":arcor.de.eu.dal.net NOTICE AUTH :*** Looking up your hostname...\r\n"
    putStrLn $ show $ parse s
    let s2 = ":arcor.de.eu.dal.net NOTICE AUTH\r\n"
    putStrLn $ show $ parse s2
    let s3 = "NOTICE AUTH\r\n"
    putStrLn $ show $ parse s3
    let s4 = "NOTICE\r\n"
    putStrLn $ show $ parse s4

address :: String -> Int -> IO AddrInfo
address hostname port = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    return $ head addrs
