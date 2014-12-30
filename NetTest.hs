{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as B
import Numeric
import Data.Char
import Data.Word
import Control.Concurrent
import Network.Socket hiding (recv)
import System.IO
import qualified System.IO.UTF8 as U
import IrcMessage as M
import IrcServer as S
import Reactive.Util as R
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

main :: IO ()
main = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just "irc.dal.net") (Just $ "7000")
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
    m <- U.hGetLine h
    putStrLn m
    putStrLn $ show $ decode Start m ""
    readLoop h

data ParseState = Start | Txt | Fg | Bg

data IrcText = Bold | Foreground Int | Background Int | Italic | Underlined | Reverse | Reset | Text String
     deriving (Show)

readColor s = if length s == 0 then 0 else read s :: Int

decode :: ParseState -> [Char] -> [Char] -> [IrcText]
decode Start ('\r':cs) _ = decode Start cs ""
decode Start ('\0002':cs) _ = Bold:decode Start cs ""
decode Start ('\0003':cs) _ = decode Fg cs ""
decode Start ('\0029':cs) _ = Italic:decode Start cs ""
decode Start ('\0031':cs) _ = Underlined:decode Start cs ""
decode Start ('\0022':cs) _ = Reverse:decode Start cs ""
decode Start ('\0015':cs) _ = Reset:decode Start cs ""
decode Start (c:cs) accum = decode Txt cs [c]
decode Start [] _ = []

decode Fg (c:cs) accum | isDigit c = decode Fg cs $ accum ++ [c]
                       | c == ',' = Foreground (readColor accum):decode Bg cs ""
                       | otherwise = Foreground (readColor accum):decode Start (c:cs) ""
decode Bg (c:cs) accum | isDigit c = decode Bg cs $ accum ++ [c]
                       | otherwise = Background (readColor accum):decode Start (c:cs) ""

decode Txt ('\r':cs) accum = decode Txt cs accum
decode Txt ('\0002':cs) accum = Text accum:Bold:decode Start cs ""
decode Txt ('\0003':cs) accum = Text accum:decode Fg cs ""
decode Txt ('\0029':cs) accum = Text accum:Italic:decode Start cs ""
decode Txt ('\0031':cs) accum = Text accum:Underlined:decode Start cs ""
decode Txt ('\0022':cs) accum = Text accum:Reverse:decode Start cs ""
decode Txt ('\0015':cs) accum = Text accum:decode Start cs ""
decode Txt (c:cs) accum = decode Txt cs $ accum ++ [c]
decode Txt [] accum = [Text accum]
