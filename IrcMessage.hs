module IrcMessage
    (
      Message(..)
    , parse
    ) where

data Message = Message { sender   :: Maybe String
                       , command  :: String
                       , params   :: [String]
                       } deriving (Show)

data ParseState = Start | Prefix | Cmd | ParamS | ParamM | ParamT
data ParseToken = Sender String | Command String | Param String
                  deriving (Show)

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
parseM Start (':':cs) accum = parseM Prefix cs ""
parseM Prefix (' ':cs) accum = (Sender accum:parseM Cmd cs "")
parseM Prefix (c:cs) accum = parseM Prefix cs $ accum ++ [c]
parseM Cmd (' ':cs) accum = (Command accum:parseM ParamS cs "")
parseM Cmd (c:cs) accum = parseM Cmd cs $ accum ++ [c]
parseM ParamS (':':cs) accum = parseM ParamT cs ""
parseM ParamS (c:cs) accum = parseM ParamM cs [c]
parseM ParamM (' ':cs) accum = (Param accum:parseM ParamS cs "")
parseM ParamM (c:cs) accum = parseM ParamM cs $ accum ++ [c]
parseM ParamT [] accum = [Param accum]
parseM ParamT ('\r':cs) accum = [Param accum]
parseM ParamT (c:cs) accum = parseM ParamT cs $ accum ++ [c]
parseM _ _ _ = []
