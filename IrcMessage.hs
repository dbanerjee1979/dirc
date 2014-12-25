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
    , parse
    , generate
    ) where

import Data.Maybe
import Data.List

data Message = Nick { nickname :: String }
               | User           { username :: String, modeMask :: Int, realname :: String }
               | Notice         { sender :: Maybe String, target :: String, text :: String }
               | Mode           { sender :: Maybe String, nickname :: String, mode :: String }
               | Generic        { sender :: Maybe String, target :: String, text :: String }
               | Welcome        { sender :: Maybe String, target :: String, text :: String }
               | YourHost       { sender :: Maybe String, target :: String, text :: String }
               | Created        { sender :: Maybe String, target :: String, text :: String }
               | MyInfo         { sender :: Maybe String, target :: String, server :: String, version :: String, availUserModes :: String, availChanModes :: String }
               | Bounce         { sender :: Maybe String, target :: String, text :: String }
               | UserClient     { sender :: Maybe String, target :: String, text :: String }
               | UserOps        { sender :: Maybe String, target :: String, cnt :: Int, text :: String }
               | UserUnk        { sender :: Maybe String, target :: String, cnt :: Int, text :: String }
               | UserChan       { sender :: Maybe String, target :: String, cnt :: Int, text :: String }
               | UserMe         { sender :: Maybe String, target :: String, text :: String }
               | MotDStart      { sender :: Maybe String, target :: String, text :: String }
               | MotD           { sender :: Maybe String, target :: String, text :: String }
               | MotDEnd        { sender :: Maybe String, target :: String, text :: String }
               | MotDNone       { sender :: Maybe String, target :: String, text :: String }
               deriving (Show)

data ParseState = Start | Prefix | Cmd | ParamS | ParamM | ParamT
data ParseToken = Sender String | Command String | Param String
                  deriving (Show)

generate :: Message -> String
generate (Nick nickname)                 = makeMsg ["NICK", nickname]
generate (User user modeMask realname)   = makeMsg ["USER", user, (show modeMask), "*", realname]

makeMsg :: [String] -> String
makeMsg (midP:termP:[]) = midP ++ " :" ++ termP ++ "\r\n"
makeMsg (p:[]) = p ++ "\r\n"
makeMsg (p:ps) = p ++ " " ++ makeMsg ps

paramT :: String -> String
paramT p = ":" ++ p

parse :: String -> Maybe Message
parse msg = case (parseM Start msg "") of
    (Sender s:Command "NOTICE":Param target:Param text:[])                 -> Just $ Notice         { sender = (Just s), target = target, text = text }
    (Sender s:Command   "MODE":Param nickname:Param mode:[])               -> Just $ Mode           { sender = (Just s), nickname = nickname, mode = mode }
    (Sender s:Command    "001":Param target:Param text:[])                 -> Just $ Welcome        { sender = (Just s), target = target, text = text }
    (Sender s:Command    "002":Param target:Param text:[])                 -> Just $ YourHost       { sender = (Just s), target = target, text = text }
    (Sender s:Command    "003":Param target:Param text:[])                 -> Just $ Created        { sender = (Just s), target = target, text = text }
    (Sender s:Command    "004":Param target:Param servername
                              :Param version:Param availUserModes
                              :Param availChanModes:[])                    -> Just $ MyInfo         { sender = (Just s), target = target, server = servername, version = version
                                                                                                    , availUserModes = availUserModes, availChanModes = availChanModes
                                                                                                    }
    (Sender s:Command    "005":Param target:Param text:[])                 -> Just $ Bounce         { sender = (Just s), target = target, text = text }
    (Sender s:Command    "251":Param target:Param text:[])                 -> Just $ Generic        { sender = (Just s), target = target, text = text }
    (Sender s:Command    "252":Param target:Param cnt:Param text:[])       -> Just $ Generic        { sender = (Just s), target = target, text = cnt ++ " " ++ text }
    (Sender s:Command    "253":Param target:Param cnt:Param text:[])       -> Just $ Generic        { sender = (Just s), target = target, text = cnt ++ " " ++ text }
    (Sender s:Command    "254":Param target:Param cnt:Param text:[])       -> Just $ Generic        { sender = (Just s), target = target, text = cnt ++ " " ++ text }
    (Sender s:Command    "255":Param target:Param text:[])                 -> Just $ Generic        { sender = (Just s), target = target, text = text }
    (Sender s:Command    "375":Param target:Param text:[])                 -> Just $ MotDStart      { sender = (Just s), target = target, text = text }
    (Sender s:Command    "372":Param target:Param text:[])                 -> Just $ MotD           { sender = (Just s), target = target, text = text }
    (Sender s:Command    "376":Param target:Param text:[])                 -> Just $ MotDEnd        { sender = (Just s), target = target, text = text }
    (Sender s:Command    "422":Param target:Param text:[])                 -> Just $ MotDNone       { sender = (Just s), target = target, text = text }
    _                                                                      -> Nothing


-- Parse prefix (if present) - from ':' to space
parseM Start (':':cs) accum = parseM Prefix cs ""
parseM Prefix (' ':cs) accum = (Sender accum:parseM Cmd cs "")
parseM Prefix (c:cs) accum = parseM Prefix cs $ accum ++ [c]

-- Parse command (after prefix - if present, or at start) - until space
parseM Start (c:cs) accum = parseM Cmd cs [c]
parseM Cmd (' ':cs) accum = (Command accum:parseM ParamS cs "")
parseM Cmd (c:cs) accum = parseM Cmd cs $ accum ++ [c]

-- Parse parameter - trailing parameter starts with ':', middle parameters starts with other character
parseM ParamS (':':cs) accum = parseM ParamT cs ""
parseM ParamS (c:cs) accum = parseM ParamM cs [c]

-- Parse middle parameter, until space, end or carriage return
parseM ParamM [] accum = [Param accum]
parseM ParamM ('\r':cs) accum = [Param accum]
parseM ParamM (' ':cs) accum = (Param accum:parseM ParamS cs "")
parseM ParamM (c:cs) accum = parseM ParamM cs $ accum ++ [c]

-- Parse trailing parameter, until end or carriage return
parseM ParamT [] accum = [Param accum]
parseM ParamT ('\r':cs) accum = [Param accum]
parseM ParamT (c:cs) accum = parseM ParamT cs $ accum ++ [c]

-- Invalid message (or unanticipated state) - terminate parse
parseM _ _ _ = []
