{-
 - Module to parse and generate IRC command messages
 -
 - This module parses and generates IRC command message strings according to the specification in the RFC
 -
 -     https://tools.ietf.org/html/rfc2812#section-2.3
 -
 - through Message, a data structure representing the contents of the message.
 -}
module IrcMessage
    (
      Message(..)
    , IrcText(..)
    , parseMsg
    , generate
    ) where

import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char
import Text.Parsec.Combinator

data Message = Nick { nickname :: String }
               | User           { username :: String, modeMask :: Int, realname :: String }
               | List
               | Join           { channel :: String }
               | Notice         { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | Mode           { sender :: Maybe String, nickname :: String, mode :: String }
               | Generic        { sender :: Maybe String, target :: String, text :: String }
               | Welcome        { sender :: Maybe String, target :: String, text :: String }
               | YourHost       { sender :: Maybe String, target :: String, text :: String }
               | Created        { sender :: Maybe String, target :: String, text :: String }
               | MyInfo         { sender :: Maybe String, target :: String, server :: String, version :: String, availUserModes :: String, availChanModes :: String }
               | Bounce         { sender :: Maybe String, target :: String, text :: String }
               | MotDStart      { sender :: Maybe String, target :: String, text :: String }
               | MotD           { sender :: Maybe String, target :: String, text :: String }
               | MotDEnd        { sender :: Maybe String, target :: String, text :: String }
               | MotDNone       { sender :: Maybe String, target :: String, text :: String }
               deriving (Show)

data IrcText = Bold | Italic | Underlined | Reverse | Reset | Foreground Int | Background Int | Text String
               deriving (Show)

data ParseToken = Sender String (Maybe String) (Maybe String) | Command String | Param String
                  deriving (Show)

generate :: Message -> String
generate (Nick nickname)                 = makeMsg ["NICK", nickname]
generate (User user modeMask realname)   = makeMsg ["USER", user, (show modeMask), "*", realname]
generate (List)                          = makeMsg ["LIST"]
generate (Join channel)                  = makeMsg ["JOIN", channel]

makeMsg :: [String] -> String
makeMsg (midP:termP:[]) = midP ++ " :" ++ termP ++ "\r\n"
makeMsg (p:[]) = p ++ "\r\n"
makeMsg (p:ps) = p ++ " " ++ makeMsg ps

paramT :: String -> String
paramT p = ":" ++ p

parseMsg :: String -> Either String Message
parseMsg input = case p of
    Right (Sender s _ _:Command "NOTICE":Param target:Param text:[])                 -> Right $ Notice         { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command   "MODE":Param nickname:Param mode:[])               -> Right $ Mode           { sender = (Just s), nickname = nickname, mode = mode }
    Right (Sender s _ _:Command    "001":Param target:Param text:[])                 -> Right $ Welcome        { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "002":Param target:Param text:[])                 -> Right $ YourHost       { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "003":Param target:Param text:[])                 -> Right $ Created        { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "004":Param target:Param servername
                                        :Param version:Param availUserModes
                                        :Param availChanModes:[])                    -> Right $ MyInfo         { sender = (Just s), target = target, server = servername, version = version
                                                                                                               , availUserModes = availUserModes, availChanModes = availChanModes
                                                                                                               }
    Right (Sender s _ _:Command    "005":Param target:Param text:[])                 -> Right $ Bounce         { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "251":Param target:Param text:[])                 -> Right $ Generic        { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "252":Param target:Param cnt:Param text:[])       -> Right $ Generic        { sender = (Just s), target = target, text = cnt ++ " " ++ text }
    Right (Sender s _ _:Command    "253":Param target:Param cnt:Param text:[])       -> Right $ Generic        { sender = (Just s), target = target, text = cnt ++ " " ++ text }
    Right (Sender s _ _:Command    "254":Param target:Param cnt:Param text:[])       -> Right $ Generic        { sender = (Just s), target = target, text = cnt ++ " " ++ text }
    Right (Sender s _ _:Command    "255":Param target:Param text:[])                 -> Right $ Generic        { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "375":Param target:Param text:[])                 -> Right $ MotDStart      { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "372":Param target:Param text:[])                 -> Right $ MotD           { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "376":Param target:Param text:[])                 -> Right $ MotDEnd        { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "422":Param target:Param text:[])                 -> Right $ MotDNone       { sender = (Just s), target = target, text = text }
    Left error                                                                       -> Left $ show error
    _                                                                                -> Left $ "Unable to parse " ++ input ++ "\n (parse: " ++ (show p) ++ ")"
    where p = parse ircMessage "(unknown)" input


ircMessage = (do char ':'
                 prefix <- prefix
                 char ' '
                 command <- command
                 return $ prefix:command) <|> command

prefix = do prefix <- many segment
            user <- many (char '!' >> many segment)
            host <- many (char '@' >> many segment)
            return $ Sender prefix (listToMaybe user) (listToMaybe host)

segment = noneOf "!@ "

command = do cmd <- many (letter <|> digit)
             params <- many param
             return $ (Command cmd):params

param = char ' ' >> (trailing <|> middle)

trailing = do char ':'
              param <- manyTill anyChar $ try $ char '\r'
              return $ Param param

middle = do c <- noneOf "\n\r :"
            cs <- many (char ':' <|> noneOf "\n\r :")
            return $ Param (c:cs)

parseText :: String -> [IrcText]
parseText input = case p of
                      Left _    -> [Text input]
                      Right msg -> msg
                  where p = parse formattedText "(unknown)" input

formattedText = do cs <- many $ (do char '\x0002'
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
                                    backgroundNum <- many $ char ',' >> many1 digit
                                    let toForeground ns = map (Foreground . read) ns
                                        toBackground ns = map (Background . read) ns
                                    return $ concat [toForeground foregroundNum, toBackground backgroundNum])
                            <|> (do cs <- many1 $ noneOf "\x0002\x001D\x001F\x0016\x000F\x0003\r"
                                    return $ [Text cs])
                   return $ concat cs
