{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Network.Socket
import System.IO
import System.Exit
import Control.Concurrent
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

    let fgColor name = do tag <- textTagNew Nothing
                          set tag [ textTagForeground := name ]
                          textTagTableAdd tagTbl tag
                          return tag
    fgColor0 <- fgColor "white"
    fgColor1 <- fgColor "black"
    fgColor2 <- fgColor "navy"
    fgColor3 <- fgColor "green"
    fgColor4 <- fgColor "red"
    fgColor5 <- fgColor "brown"
    fgColor6 <- fgColor "purple"
    fgColor7 <- fgColor "olive drab"
    fgColor8 <- fgColor "yellow"
    fgColor9 <- fgColor "lime green"
    fgColor10 <- fgColor "turquoise4"
    fgColor11 <- fgColor "cyan1"
    fgColor12 <- fgColor "blue"
    fgColor13 <- fgColor "magenta"
    fgColor14 <- fgColor "gray55"
    fgColor15 <- fgColor "gray90"
    fgColor16 <- fgColor "white"
    let fgColorMap = Map.fromList [ (0, fgColor0)
                                  , (1, fgColor1)
                                  , (2, fgColor2)
                                  , (3, fgColor3)
                                  , (4, fgColor4)
                                  , (5, fgColor5)
                                  , (6, fgColor6)
                                  , (7, fgColor7)
                                  , (8, fgColor8)
                                  , (9, fgColor9)
                                  , (10, fgColor10)
                                  , (11, fgColor11)
                                  , (12, fgColor12)
                                  , (13, fgColor13)
                                  , (14, fgColor14)
                                  , (15, fgColor15)
                                  , (16, fgColor16)
                                  ]

    let bgColor name = do tag <- textTagNew Nothing
                          set tag [ textTagBackground := name ]
                          textTagTableAdd tagTbl tag
                          return tag
    bgColor0 <- bgColor "white"
    bgColor1 <- bgColor "black"
    bgColor2 <- bgColor "navy"
    bgColor3 <- bgColor "green"
    bgColor4 <- bgColor "red"
    bgColor5 <- bgColor "brown"
    bgColor6 <- bgColor "purple"
    bgColor7 <- bgColor "olive drab"
    bgColor8 <- bgColor "yellow"
    bgColor9 <- bgColor "lime green"
    bgColor10 <- bgColor "turquoise4"
    bgColor11 <- bgColor "cyan1"
    bgColor12 <- bgColor "blue"
    bgColor13 <- bgColor "magenta"
    bgColor14 <- bgColor "gray55"
    bgColor15 <- bgColor "gray90"
    bgColor16 <- bgColor "white"
    let bgColorMap = Map.fromList [ (0, bgColor0)
                                  , (1, bgColor1)
                                  , (2, bgColor2)
                                  , (3, bgColor3)
                                  , (4, bgColor4)
                                  , (5, bgColor5)
                                  , (6, bgColor6)
                                  , (7, bgColor7)
                                  , (8, bgColor8)
                                  , (9, bgColor9)
                                  , (10, bgColor10)
                                  , (11, bgColor11)
                                  , (12, bgColor12)
                                  , (13, bgColor13)
                                  , (14, bgColor14)
                                  , (15, bgColor15)
                                  , (16, bgColor16)
                                  ]

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
