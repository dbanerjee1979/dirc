{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Network.Socket
import System.IO
import System.Exit
import Control.Concurrent

import Graphics.UI.Gtk as Gtk hiding (Event, insertText)
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
    fontTag <- textTagNew Nothing
    set fontTag [ textTagFont := "Courier 12" ]
    textTagTableAdd tagTbl fontTag
    motdTag <- textTagNew Nothing
    set motdTag [ textTagParagraphBackground := "yellow", textTagForeground := "blue", textTagWeight := 700 ]
    textTagTableAdd tagTbl motdTag

    esmsg <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eclose <- eventM closeBtn Gtk.buttonReleaseEvent
            edelete <- eventM dlg Gtk.deleteEvent
            emsg <- fromAddHandler (addHandler esmsg)

            reactimate $ (postGUIAsync . handleMsg buffer fontTag motdTag) <$> emsg
            reactimate $ handleQuit' <$ eclose
            reactimate $ handleQuit' <$ edelete
    network <- compile networkDescription
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

handleQuit exit = do
    putMVar exit ExitSuccess

data MsgPart = TextIcon String | Text [TextTag] String

handleMsg :: TextBufferClass self => self -> TextTag -> TextTag -> Message -> IO ()
handleMsg buffer fontTag motdTag (Notice sender "AUTH" text) = insertMsg buffer [TextIcon "icon-auth.svg", Text [fontTag] text]
handleMsg buffer fontTag motdTag (Notice sender nickname text) = insertMsg buffer [TextIcon "icon-info.svg", Text [fontTag] text]
handleMsg buffer fontTag motdTag (Generic sender nickname text) = insertMsg buffer [Text [fontTag] text]
handleMsg buffer fontTag motdTag (Welcome sender nickname text) = insertMsg buffer [Text [fontTag] text]
handleMsg buffer fontTag motdTag (YourHost sender nickname text) = insertMsg buffer [Text [fontTag] text]
handleMsg buffer fontTag motdTag (Created sender nickname text) = insertMsg buffer [Text [fontTag] text]
handleMsg buffer fontTag motdTag (MotD sender nickname text) = insertMsg buffer [Text [fontTag, motdTag] text]
handleMsg buffer fontTag motdTag (MotDStart sender nickname text) = insertMsg buffer [Text [fontTag, motdTag] " "]
handleMsg buffer fontTag motdTag (MotDEnd sender nickname text) = insertMsg buffer [Text [fontTag, motdTag] " "]
handleMsg buffer fontTag motdTag msg = putStrLn $ show msg

insertMsg buffer (TextIcon icon:ms) = do
    b <- pixbufNewFromFile icon
    m <- textBufferGetInsert buffer
    i <- textBufferGetIterAtMark buffer m
    textBufferInsertPixbuf buffer i b
    textBufferInsertAtCursor buffer " "
    insertMsg buffer ms

insertMsg buffer (Text tags text:ms) = do
    m <- textBufferGetInsert buffer
    i <- textBufferGetIterAtMark buffer m
    o <- textIterGetOffset i
    textBufferInsertAtCursor buffer text
    i1 <- textBufferGetIterAtOffset buffer o
    i2 <- textBufferGetIterAtMark buffer m
    applyTags buffer tags i1 i2
    insertMsg buffer ms

insertMsg buffer [] = do
    textBufferInsertAtCursor buffer "\n"

applyTags buffer (t:ts) i1 i2 = do
    textBufferApplyTag buffer t i1 i2
    applyTags buffer ts i1 i2
applyTags buffer [] i1 i2 = do
    return ()
