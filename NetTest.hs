import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv) 

import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map

data Message = Message { sender :: Maybe String
                       , code   :: String
                       , params :: [String]
                       } deriving (Show)


main :: IO ()
main = do
{-
    a <- address "irc.dal.net" 7000
    s <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
    connect s (addrAddress a)
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering
    l <- hGetLine h
    putStrLn $ show l
    hClose h
    msg <- recv s 1024
    putStrLn $ foo
    sClose s
-}
    let s = ":arcor.de.eu.dal.net NOTICE AUTH :*** Looking up your hostname...\r\n"
    putStrLn $ show $ parse s
    let s2 = ":arcor.de.eu.dal.net NOTICE AUTH\r\n"
    putStrLn $ show $ parse s2
    let s3 = "NOTICE AUTH\r\n"
    putStrLn $ show $ parse s3
    let s4 = "NOTICE\r\n"
    putStrLn $ show $ parse s4

data ParseState = Start | Name | Code | Param | ParamM | ParamT
data ValueType = One String | Many [String]

parse :: String -> Maybe Message
parse []  = Nothing    
parse m  = build Start m "" $ Map.fromList [("params", (Many []))]

build :: ParseState -> String -> String -> Map.Map String ValueType -> Maybe Message
build Start  (':':cs)  value values        = build Name cs "" values
build Start  (c:cs)    value values        = build Code cs [c] values
build Name   (' ':cs)  value values        = build Code cs "" $ appendSender value values
build Name   (c:cs)    value values        = build Name cs (value ++ [c]) values
build Code   (' ':cs)  value values        = build Param cs "" $ appendCode value values
build Code   ('\r':cs) value values        = makeMessage $ appendCode value values
build Code   ('\n':cs) value values        = makeMessage $ appendCode value values
build Code   (c:cs)    value values        = build Code cs (value ++ [c]) values
build Param  (':':cs)  value values        = build ParamT cs "" values 
build Param  (c:cs)    value values        = build ParamM cs [c] values
build ParamT ('\r':cs) value values        = makeMessageParams value values
build ParamT ('\n':cs) value values        = makeMessageParams value values
build ParamT (c:cs)    value values        = build ParamT cs (value ++ [c]) values
build ParamM (' ':cs)  value values        = build Param cs "" (appendParams value values)
build ParamM ('\r':cs) value values        = makeMessageParams value values
build ParamM ('\n':cs) value values        = makeMessageParams value values
build ParamM (c:cs)    value values        = build ParamM cs (value ++ [c]) values
build _      _         _     _             = Nothing

appendSender:: String -> Map.Map String ValueType -> Map.Map String ValueType
appendSender value values = Map.insert "sender" (One value) values

appendCode :: String -> Map.Map String ValueType -> Map.Map String ValueType
appendCode value values = Map.insert "code" (One value) values

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
    code <- Map.lookup "code" values
    params <- Map.lookup "params" values
    return Message { sender = do v <- Map.lookup "sender" values
                                 return $ extract v
                   , code = extract $ code
                   , params = extractAll $ params
                   }

extract :: ValueType -> String
extract (One a) = a

extractAll :: ValueType -> [String]
extractAll (Many a) = a

{-
build Name  ' ' cs sender code param params        = build Start (C.head cs) (B.tail cs) sender code param params
build Name  c   cs (Just sender) code param params = build Name (C.head cs) (B.tail cs) (Just $ sender ++ [c]) code param params
build Code  ' ' cs sender code                     = 
build Code  ' ' cs sender code              = Just Message { sender = sender
                                                           , code = code
                                                           }
build Code  c   cs sender code              = build Code (C.head cs) (B.tail cs) sender (code ++ [c])
-}

address :: String -> Int -> IO AddrInfo
address hostname port = do
    let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG, AI_CANONNAME ] }
    addrs <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    return $ head addrs
