{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import IrcMessage as M
import IrcServer as S
import Reactive.Util as R
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

main :: IO ()
main = do
    esmsg <- newAddHandler
    network <- compile $ setupNetwork (esmsg)
    actuate network

    h <- S.startServer "irc.dal.net" 7000 esmsg
    readLoop h

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
