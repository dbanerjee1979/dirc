module IrcMessage
    (
      Message(..)
    , parse
    ) where

import qualified Data.Map.Strict as Map

data Message = Message { sender   :: Maybe String
                       , command  :: String
                       , params   :: [String]
                       } deriving (Show)

data ParseState = Start | Name | Command | Param | ParamM | ParamT
data ValueType = One String | Many [String]

parse :: String -> Maybe Message
parse [] = Nothing    
parse m  = build Start m "" $ Map.fromList [("params", (Many []))]

build :: ParseState -> String -> String -> Map.Map String ValueType -> Maybe Message
build Start   (':':cs)  value values        = build Name cs "" values
build Start   (c:cs)    value values        = build Command cs [c] values
build Name    (' ':cs)  value values        = build Command cs "" $ appendSender value values
build Name    (c:cs)    value values        = build Name cs (value ++ [c]) values
build Command (' ':cs)  value values        = build Param cs "" $ appendCommand value values
build Command ('\r':cs) value values        = makeMessage $ appendCommand value values
build Command ('\n':cs) value values        = makeMessage $ appendCommand value values
build Command (c:cs)    value values        = build Command cs (value ++ [c]) values
build Param   (':':cs)  value values        = build ParamT cs "" values 
build Param   (c:cs)    value values        = build ParamM cs [c] values
build ParamT  ('\r':cs) value values        = makeMessageParams value values
build ParamT  ('\n':cs) value values        = makeMessageParams value values
build ParamT  (c:cs)    value values        = build ParamT cs (value ++ [c]) values
build ParamM  (' ':cs)  value values        = build Param cs "" (appendParams value values)
build ParamM  ('\r':cs) value values        = makeMessageParams value values
build ParamM  ('\n':cs) value values        = makeMessageParams value values
build ParamM  (c:cs)    value values        = build ParamM cs (value ++ [c]) values
build _       _         _     _             = Nothing

appendSender:: String -> Map.Map String ValueType -> Map.Map String ValueType
appendSender value values = Map.insert "sender" (One value) values

appendCommand :: String -> Map.Map String ValueType -> Map.Map String ValueType
appendCommand value values = Map.insert "command" (One value) values

appendParams :: String -> Map.Map String ValueType -> Map.Map String ValueType
appendParams value values = Map.insert "params" (Many v) values
                            where v = case Map.lookup "params" values of
                                          Nothing -> [value]
                                          Just (Many params) -> params ++ [value]

makeMessageParams :: String -> Map.Map String ValueType -> Maybe Message
makeMessageParams value values =
    let values' = appendParams value values
    in makeMessage values'

makeMessage ::  Map.Map String ValueType -> Maybe Message
makeMessage values = do 
    command <- Map.lookup "command" values
    params <- Map.lookup "params" values
    return Message { sender   = do v <- Map.lookup "sender" values
                                   return $ extract v
                   , command = extract $ command
                   , params  = extractAll $ params
                   }

extract :: ValueType -> String
extract (One a) = a

extractAll :: ValueType -> [String]
extractAll (Many a) = a
