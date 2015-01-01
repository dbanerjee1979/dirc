{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Network.Socket
import System.IO
import System.Exit
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Word
import qualified Data.Map.Strict as Map

import Graphics.UI.Gtk as Gtk hiding (Event)
import Graphics.UI.Gtk.Builder

import Reactive.Util
import Reactive.Banana
import Reactive.Banana.Gtk

import IrcServer as S
import IrcMessage as M

data MsgPart = MsgIcon String | MsgText [TextTag] String

main :: IO ()
main = do
    initGUI

    bld <- builderNew
    builderAddFromFile bld "dirc.glade"
    dlg <- builderGetObject bld castToWindow "main-dialog"
    closeBtn <- builderGetObject bld castToButton "close-button"
    msgTxt <- builderGetObject bld castToTextView "message-text"
    buffer <- textViewGetBuffer msgTxt
    tagTbl <- textBufferGetTagTable buffer
    fontTag <- textTagNew Nothing
    set fontTag [ textTagFont := "Courier 12" ]
    textTagTableAdd tagTbl fontTag
    motdTag <- textTagNew Nothing
    set motdTag [ textTagParagraphBackground := "yellow", textTagWeight := 700 ]
    textTagTableAdd tagTbl motdTag
    boldTag <- textTagNew Nothing
    set boldTag [ textTagWeight := 700 ]
    textTagTableAdd tagTbl boldTag
    ulTag <- textTagNew Nothing
    set ulTag [ textTagUnderline := UnderlineSingle ]
    textTagTableAdd tagTbl ulTag
    italicTag <- textTagNew Nothing
    set italicTag [ textTagStyle := StyleItalic ]
    textTagTableAdd tagTbl italicTag

    let colors = [ "white", "black", "navy", "green", "red", "brown"
                 , "purple", "olive drab", "yellow", "lime green", "turquoise4"
                 , "cyan1", "blue", "magenta", "gray55", "gray90", "white"
                 ]

    let insertTag attr num name map = do tag <- textTagNew Nothing
                                         set tag [ attr := name ]
                                         textTagTableAdd tagTbl tag
                                         return $ Map.insert num tag map

    let mkColorMap f = foldM (\m (k, v) -> f k v m) Map.empty $ zip [0..] colors

    fgColorMap <- mkColorMap $ insertTag textTagForeground
    bgColorMap <- mkColorMap $ insertTag textTagBackground

    exit <- newEmptyMVar
    esmsg <- newAddHandler
    esquit <- newAddHandler
    let handler = tryEvent $ do liftIO $ fire esquit ()
    closeBtn `on` Gtk.buttonReleaseEvent $ handler
    dlg `on` Gtk.deleteEvent $ handler

    let toMsg :: [Int] -> [TextTag] -> [IrcText] -> [MsgPart]
        toMsg []           tags (Text text:ms)    = (MsgText tags text:toMsg [] tags ms)
        toMsg (fgc:bgc:[]) tags (Text text:ms)    = let fgTag = maybeToList $ Map.lookup fgc fgColorMap
                                                        bgTag = maybeToList $ Map.lookup bgc bgColorMap
                                                    in (MsgText (concat [fgTag, bgTag, tags]) text:toMsg [fgc, bgc] tags ms)
        toMsg colors tags (Bold:ms)               = toMsg colors (boldTag:tags) ms
        toMsg colors tags (Underlined:ms)         = toMsg colors (ulTag:tags) ms
        toMsg colors tags (Italic:ms)             = toMsg colors (italicTag:tags) ms
        toMsg colors tags (Reset:ms)              = toMsg colors [fontTag] ms
        toMsg (_:bgc:[]) tags (Foreground c:ms)   = toMsg (c:bgc:[]) tags ms
        toMsg (fgc:_:[]) tags (Background c:ms)   = toMsg (fgc:c:[]) tags ms
        toMsg (fgc:bgc:[]) tags (Reverse:ms)      = toMsg (bgc:fgc:[]) tags ms
        toMsg colors tags []                      = []

    let handleMsg msg = postGUIAsync $ do
            case msg of
                (Notice sender target text)                 -> insertMsg (MsgIcon "icon-info.svg":toMsg [1, 0] [fontTag] text)
                (Generic sender nickname text)              -> insertMsg $ toMsg [1, 0] [fontTag] text
                (Welcome sender nickname text)              -> insertMsg $ toMsg [1, 0] [fontTag] text
                (YourHost sender nickname text)             -> insertMsg $ toMsg [1, 0] [fontTag] text
                (Created sender nickname text)              -> insertMsg $ toMsg [1, 0] [fontTag] text
                (MotD sender nickname text)                 -> insertMsg $ toMsg [2, 8] [fontTag, motdTag] text
                (MotDStart sender nickname text)            -> insertMsg $ toMsg [2, 8] [fontTag, motdTag] [Text " "]
                (MotDEnd sender nickname text)              -> insertMsg $ toMsg [2, 8] [fontTag, motdTag] [Text " "]
                (Channel sender target channel _ topic)     -> insertMsg $ MsgText [fontTag] (channel ++ ": "):toMsg [1, 0] [fontTag] topic
                msg                                         -> putStrLn $ show msg
        insertMsg msg = do
            m <- textBufferGetInsert buffer
            i <- textBufferGetIterAtMark buffer m
            case msg of
                (MsgIcon icon:ms)      -> do b <- pixbufNewFromFile icon
                                             textBufferInsertPixbuf buffer i b
                                             textBufferInsertAtCursor buffer " "
                                             insertMsg ms
                (MsgText tags text:ms) -> do o <- textIterGetOffset i
                                             textBufferInsertAtCursor buffer text
                                             i1 <- textBufferGetIterAtOffset buffer o
                                             i2 <- textBufferGetIterAtMark buffer m
                                             let applyTags (t:ts) = do textBufferApplyTag buffer t i1 i2
                                                                       applyTags ts
                                                 applyTags []     = do return ()
                                             applyTags tags
                                             insertMsg ms
                []                     -> textBufferInsertAtCursor buffer "\n"
        handleQuit = putMVar exit ExitSuccess

    network <- compile $ setupNetwork (esmsg, esquit) handleMsg handleQuit
    actuate network

    sChan <- newChan
    --S.startServer "irc.freenode.net" 6665 esmsg sChan
    S.startServer "irc.dal.net" 7000 esmsg sChan
    writeChan sChan M.Nick { nickname = "dbanerjee1979" }
    writeChan sChan M.User { username = "guest", modeMask = 0, realname = "Joe" }
    writeChan sChan M.List { channelFilter = Nothing }

    widgetShowAll dlg
    forkOS mainGUI
    signal <- takeMVar exit
    postGUIAsync mainQuit
    exitWith signal

setupNetwork :: forall t. Frameworks t => (EventSource Message, EventSource ()) -> (Message -> IO ()) -> IO () -> Moment t ()
setupNetwork (esmsg, esquit) handleMsg handleQuit = do
    emsg <- fromAddHandler (addHandler esmsg)
    equit <- fromAddHandler (addHandler esquit)
    reactimate $ handleMsg <$> emsg
    reactimate $ handleQuit <$ equit
