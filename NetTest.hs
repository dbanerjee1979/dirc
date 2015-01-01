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
    --putStrLn $ show $ parseMsg ":halcyon.il.us.dal.net 322 dbanerjee1979 #personal 6 :\ETX0,\r"
    putStrLn $ show $ parseMsg ":halcyon.il.us.dal.net 322 dbanerjee1979 #personal 6 :\ETX0,\ETX7 WELCOMEl  Aye Ishq Humain Barbad Na Kar .. Barbad Na kar..\r"
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    --addrs <- getAddrInfo (Just hints) (Just "chat.freenode.net") (Just $ "6665")
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
    putStrLn $ show $ parseMsg m
    readLoop h

data IrcMessage = Prefix String (Maybe String) (Maybe String) | Command String | Param [IrcText]
                  deriving (Show)

data IrcText = Bold | Italic | Underlined | Reverse | Reset | Foreground Int | Background Int | Text String
               deriving (Show)

parseMsg input = parse ircMessage "(unknown)" input

ircMessage = (do char ':'
                 prefix <- prefix
                 command <- command
                 return $ prefix:command) <|> command

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
              text <- parseText
              char '\r'
              return $ Param $ concat text

middle = do c <- noneOf " :"
            cs <- many (char ':' <|> noneOf " :")
            return $ Param $ [Text (c:cs)]

parseText = many $ (do char '\x0002'
                       return [Bold])
                   <|> (do char '\x001D'
                           return [Italic])
                   <|> (do char '\x001F'
                           return [Underlined])
                   <|> (do char '\x0016'
                           return [Reverse])
                   <|> (do char '\x000F'
                           return [Reset])
                   <|> (do char '\x0003'
                           foregroundNum <- many $ many1 digit
                           backgroundNum <- many $ char ',' >> many digit
                           let toForeground ns = map (Foreground . read) ns
                               toBackground ns = map (Background . read) ns
                           return $ concat [toForeground foregroundNum, toBackground $ filter (not . null) backgroundNum])
                   <|> (do cs <- many1 $ noneOf "\x0002\x001D\x001F\x0016\x000F\x0003\r"
                           return $ [Text cs])
