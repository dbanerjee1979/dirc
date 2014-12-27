{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Network.Socket
import System.IO
import System.Exit
import Control.Concurrent

import Graphics.UI.Gtk as Gtk hiding (Event)
import Graphics.UI.Gtk.Builder

import Reactive.Util
import Reactive.Banana
import Reactive.Banana.Gtk

import IrcServer as S
import IrcMessage as M

data MsgPart = TextIcon String | Text [TextTag] String

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
    set motdTag [ textTagParagraphBackground := "yellow", textTagForeground := "blue", textTagWeight := 700 ]
    textTagTableAdd tagTbl motdTag

    exit <- newEmptyMVar
    esmsg <- newAddHandler
    esquit <- newAddHandler
    let handler = tryEvent $ do liftIO $ fire esquit ()
    closeBtn `on` Gtk.buttonReleaseEvent $ handler
    dlg `on` Gtk.deleteEvent $ handler

    let handleMsg msg = postGUIAsync $ do
            case msg of
                (Notice sender "AUTH" text)          -> insertMsg [TextIcon "icon-auth.svg", Text [fontTag] text]
                (Notice sender nickname text)        -> insertMsg [TextIcon "icon-info.svg", Text [fontTag] text]
                (Generic sender nickname text)       -> insertMsg [Text [fontTag] text]
                (Welcome sender nickname text)       -> insertMsg [Text [fontTag] text]
                (YourHost sender nickname text)      -> insertMsg [Text [fontTag] text]
                (Created sender nickname text)       -> insertMsg [Text [fontTag] text]
                (MotD sender nickname text)          -> insertMsg [Text [fontTag, motdTag] text]
                (MotDStart sender nickname text)     -> insertMsg [Text [fontTag, motdTag] " "]
                (MotDEnd sender nickname text)       -> insertMsg [Text [fontTag, motdTag] " "]
                msg                                  -> putStrLn $ show msg
        insertMsg msg = do
            m <- textBufferGetInsert buffer
            i <- textBufferGetIterAtMark buffer m
            case msg of
                (TextIcon icon:ms) ->  do b <- pixbufNewFromFile icon
                                          textBufferInsertPixbuf buffer i b
                                          textBufferInsertAtCursor buffer " "
                                          insertMsg ms
                (Text tags text:ms) -> do o <- textIterGetOffset i
                                          textBufferInsertAtCursor buffer text
                                          i1 <- textBufferGetIterAtOffset buffer o
                                          i2 <- textBufferGetIterAtMark buffer m
                                          let applyTags tags =
                                                  case tags of
                                                      (t:ts) -> do textBufferApplyTag buffer t i1 i2
                                                                   applyTags ts
                                                      []     -> do return ()
                                          applyTags tags
                                          insertMsg ms
                []                  -> textBufferInsertAtCursor buffer "\n"
        handleQuit = putMVar exit ExitSuccess

    network <- compile $ setupNetwork (esmsg, esquit) handleMsg handleQuit
    actuate network

    sChan <- newChan
    S.startServer "irc.dal.net" 7000 esmsg sChan
    writeChan sChan M.Nick { nickname = "dbanerjee1979" }
    writeChan sChan M.User { username = "guest", modeMask = 0, realname = "Joe" }

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
