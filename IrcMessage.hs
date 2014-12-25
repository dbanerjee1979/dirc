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

data Message = Message { sender   :: Maybe String
                       , command  :: String
                       , params   :: [String]
                       } deriving (Show)

data ParseState = Start | Prefix | Cmd | ParamS | ParamM | ParamT
data ParseToken = Sender String | Command String | Param String
                  deriving (Show)

generate :: Message -> String
generate Message { sender = s, command = c, params = ps } =
    prefix ++ command ++ params ++ "\r\n"
    where prefix = maybe "" (\x -> ":" ++ x ++ " ") s
          command = c
          params = paramStr ps

paramStr :: [String] -> String
paramStr [] = ""
paramStr (p:[]) = " :" ++ p
paramStr (p:ps) = " " ++ p ++ paramStr ps

parse :: String -> Maybe Message
parse msg = case (parseM Start msg "") of
    (Sender s:Command c:ps) -> Just Message { sender = Just s
                                            , command = c
                                            , params = paramList ps
                                            }
    (Command c:ps) ->          Just Message { sender = Nothing
                                            , command = c
                                            , params = paramList ps
                                            }
    _ ->                       Nothing
    where paramList ps = [ p | Param p <- ps ]

parseM :: ParseState -> [Char] -> [Char] -> [ParseToken]

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
