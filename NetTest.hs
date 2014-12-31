{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as B
import Numeric
import Data.Char
import Data.Word
import Data.Maybe
import Control.Concurrent
import Network.Socket hiding (recv)
import System.IO
import qualified System.IO.UTF8 as U
import IrcServer as S
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

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
    m <- U.hGetLine h
    putStrLn m
    putStrLn $ show $ parseMsg m
    readLoop h

data IrcMessage = Prefix String (Maybe String) (Maybe String) | Command String | Param String
                  deriving (Show)

parseMsg input = parse ircMessage "(unknown)" input

ircMessage = do char ':' 
                prefix <- prefix
                command <- command
                return $ prefix:command

prefix = do prefix <- many segment 
            user <- many (char '!' >> many segment)
            host <- many (char '@' >> many segment)
            return $ Prefix prefix (listToMaybe user) (listToMaybe host)

segment = noneOf "!@ "

command = do cmd <- many (letter <|> digit)
             params <- many param
             return $ (Command cmd):params

param = char ' ' >> (trailing <|> middle)

trailing = do char ':' 
              param <- manyTill anyChar $ try $ char '\r'
              return $ Param param

middle = do c <- noneOf " :"
            cs <- many (char ':' <|> noneOf " :")
            return $ Param (c:cs)
