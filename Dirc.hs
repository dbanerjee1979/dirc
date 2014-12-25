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

main :: IO ()
main = do
    initGUI
    exit <- newEmptyMVar
    let handleQuit' = handleQuit exit

    bld <- builderNew
    builderAddFromFile bld "dirc.glade"
    dlg <- builderGetObject bld castToWindow "main-dialog"
    closeBtn <- builderGetObject bld castToButton "close-button"
    msgTxt <- builderGetObject bld castToTextView "message-text"
    buffer <- textViewGetBuffer msgTxt
    tagTbl <- textBufferGetTagTable buffer
    tag <- textTagNew Nothing
    set tag [ textTagFont := "Courier 12" ]
    textTagTableAdd tagTbl tag

    esmsg <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eclose <- eventM closeBtn Gtk.buttonReleaseEvent
            edelete <- eventM dlg Gtk.deleteEvent
            emsg <- fromAddHandler (addHandler esmsg)

            reactimate $ (postGUIAsync . handleMsg buffer tag) <$> emsg
            reactimate $ handleQuit' <$ eclose
            reactimate $ handleQuit' <$ edelete
    network <- compile networkDescription
    actuate network

    sChan <- newChan
    S.startServer "irc.dal.net" 7000 esmsg sChan
    writeChan sChan M.Message { sender = Nothing, command = "NICK", params = [ "dbanerjee1979" ] }
    writeChan sChan M.Message { sender = Nothing, command = "USER", params = [ "guest", "0", "*", "Joe" ] }

    widgetShowAll dlg
    forkOS mainGUI
    signal <- takeMVar exit
    postGUIAsync mainQuit
    exitWith signal

handleQuit exit = do
    putMVar exit ExitSuccess

handleMsg :: TextBufferClass self => self -> TextTag -> Message -> IO ()
handleMsg buffer tag msg = do
    m <- textBufferGetInsert buffer
    i <- textBufferGetIterAtMark buffer m
    o <- textIterGetOffset i
    textBufferInsertAtCursor buffer $ show msg
    textBufferInsertAtCursor buffer "\n"
    i1 <- textBufferGetIterAtOffset buffer o
    i2 <- textBufferGetIterAtMark buffer m
    textBufferApplyTag buffer tag i1 i2
