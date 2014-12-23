{-# LANGUAGE ScopedTypeVariables #-}

import Network.Socket hiding (recv)
import System.IO
import Control.Concurrent
import IrcMessage as M
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

main :: IO ()
main = do
    a <- address "irc.dal.net" 7000
    putStrLn $ show a
    s <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
    connect s (addrAddress a)
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering

    esmsg <- newAddHandler
    network <- compile $ setupNetwork (esmsg)
    actuate network

    forkIO (reader h esmsg)
    readLoop h

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

handleMsg :: Message -> IO ()
handleMsg msg = do
    putStrLn $ show msg

setupNetwork :: forall t. Frameworks t => (EventSource Message) -> Moment t ()
setupNetwork (esmsg) = do
    emsg <- fromAddHandler (addHandler esmsg)
    reactimate $ handleMsg <$> emsg 

readLoop h = do
    l <- getLine
    hPutStr h l
    hPutStr h "\r\n"
    readLoop h

reader :: Handle -> EventSource Message -> IO ()
reader h esmsg = do
    loop h esmsg
    hClose h

loop h esmsg = do
    l <- hGetLine h
    case M.parse l of
        Nothing -> putStrLn $ "Unable to parse response " ++ l
        Just m -> fire esmsg m
    loop h esmsg

address :: String -> Int -> IO AddrInfo
address hostname port = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    return $ head addrs
