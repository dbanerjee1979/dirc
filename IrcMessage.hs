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

data Message = Nick { sender :: Maybe String, nickname :: String }
               | User           { username :: String, modeMask :: Int, realname :: String }
               | List           { channelFilter :: Maybe String }
               | Join           { sender :: Maybe String, channel :: String }
               | Quit           { sender :: Maybe String, msg :: [IrcText] }
               | Notice         { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | Mode           { sender :: Maybe String, nickname :: String, mode :: String }
               | Generic        { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | Welcome        { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | YourHost       { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | Created        { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | MyInfo         { sender :: Maybe String, target :: String, server :: String, version :: String, availUserModes :: String, availChanModes :: String }
               | Bounce         { sender :: Maybe String, target :: String, text :: String }
               | MotDStart      { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | MotD           { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | MotDEnd        { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | MotDNone       { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | Channel        { sender :: Maybe String, target :: String, channel :: String, visible :: String, msg :: [IrcText] }
               | Topic          { sender :: Maybe String, channel :: String, topic :: [IrcText] }
               | Names          { sender :: Maybe String, chanStat :: String, channel :: String, names :: [String] }
               | NamesEnd       { sender :: Maybe String, target :: String, channel :: String }
               | PrivMsg        { sender :: Maybe String, target :: String, msg :: [IrcText] }
               | Ping           { target :: String }
               | Pong           { sender :: Maybe String, target :: String }
               deriving (Show)

data IrcText = Bold | Italic | Underlined | Reverse | Reset | Foreground Int | Background Int | Text String
               deriving (Show)

data ParseToken = Sender String (Maybe String) (Maybe String) | Command String | Param String
                  deriving (Show)

generate :: Message -> String
generate (Nick sender nickname)          = makeMsg ["NICK", nickname]
generate (User user modeMask realname)   = makeMsg ["USER", user, (show modeMask), "*", realname]
generate (List channelFilter)            = makeMsg $ "LIST":(maybeToList channelFilter)
generate (Join sender channel)           = makeMsg ["JOIN", channel]
generate (Pong sender target)            = makeMsg ["PONG", target]

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
    Right (Sender s _ _:Command   "JOIN":Param channel:[])                           -> Right $ Join           { sender = (Just s), channel = channel }
    Right (Sender s _ _:Command   "QUIT":Param text:[])                              -> Right $ Quit           { sender = (Just s), msg = parseText $ text }
    Right (Sender s _ _:Command   "NICK":Param nickname:[])                          -> Right $ Nick           { sender = (Just s), nickname = nickname }
    Right (Sender s _ _:Command "PRIVMSG":Param target:Param text:[])                -> Right $ PrivMsg        { sender = (Just s), target = target, msg = parseText $ text }
    Right (Command   "PING":Param target:[])                                         -> Right $ Ping           { target = target }
    Right (Sender s _ _:Command    "001":Param target:Param text:[])                 -> Right $ Welcome        { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "002":Param target:Param text:[])                 -> Right $ YourHost       { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "003":Param target:Param text:[])                 -> Right $ Created        { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "004":Param target:Param servername
                                        :Param version:Param availUserModes
                                        :Param availChanModes:[])                    -> Right $ MyInfo         { sender = (Just s), target = target, server = servername, version = version
                                                                                                               , availUserModes = availUserModes, availChanModes = availChanModes
                                                                                                               }
    Right (Sender s _ _:Command    "005":Param target:Param text:[])                 -> Right $ Bounce         { sender = (Just s), target = target, text = text }
    Right (Sender s _ _:Command    "251":Param target:Param text:[])                 -> Right $ Generic        { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "252":Param target:Param cnt:Param text:[])       -> Right $ Generic        { sender = (Just s), target = target, msg = parseText $ cnt ++ " " ++ text }
    Right (Sender s _ _:Command    "253":Param target:Param cnt:Param text:[])       -> Right $ Generic        { sender = (Just s), target = target, msg = parseText $ cnt ++ " " ++ text }
    Right (Sender s _ _:Command    "254":Param target:Param cnt:Param text:[])       -> Right $ Generic        { sender = (Just s), target = target, msg = parseText $ cnt ++ " " ++ text }
    Right (Sender s _ _:Command    "255":Param target:Param text:[])                 -> Right $ Generic        { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "375":Param target:Param text:[])                 -> Right $ MotDStart      { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "372":Param target:Param text:[])                 -> Right $ MotD           { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "376":Param target:Param text:[])                 -> Right $ MotDEnd        { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "422":Param target:Param text:[])                 -> Right $ MotDNone       { sender = (Just s), target = target, msg = parseText $ text }
    Right (Sender s _ _:Command    "322":Param target:Param channel
                                        :Param visible:Param topic:[])               -> Right $ Channel        { sender = (Just s), target = target, channel = channel, visible = visible
                                                                                                               , msg = parseText $ topic
                                                                                                               }
    Right (Sender s _ _:Command    "332":Param target:Param channel
                                        :Param topic:[])                             -> Right $ Topic          { sender = (Just s), channel = channel, topic = parseText $ topic }
    Right (Sender s _ _:Command    "353":Param target:Param chanStat:Param channel
                                        :Param names:[])                             -> Right $ Names          { sender = (Just s), chanStat = chanStat, channel = channel
                                                                                                               , names = words names 
                                                                                                               }
    Right (Sender s _ _:Command    "366":Param target:Param channel:Param text:[])   -> Right $ NamesEnd       { sender = (Just s), target = target, channel = channel }
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
                                    backgroundNum <- many $ char ',' >> many digit
                                    let toForeground ns = map (Foreground . read) ns
                                        toBackground ns = map (Background . read) ns
                                    return $ concat [toForeground foregroundNum, toBackground $ filter (not . null) backgroundNum])
                            <|> (do cs <- many1 $ noneOf "\x0002\x001D\x001F\x0016\x000F\x0003\r"
                                    return $ [Text cs])
                   return $ concat cs
