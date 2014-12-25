{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Network.Socket
import System.IO

import Graphics.UI.Gtk as Gtk hiding (Event)
import Graphics.UI.Gtk.Builder
import Reactive.Banana
import Reactive.Banana.Gtk

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

main :: IO ()
main = do
    initGUI
    bld <- builderNew
    builderAddFromFile bld "dirc.glade"
    dlg <- builderGetObject bld castToWindow "main-dialog"
    closeBtn <- builderGetObject bld castToButton "close-button"
    msgTxt <- builderGetObject bld castToTextView "message-text"
    buffer <- textViewGetBuffer msgTxt

    esmsg <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eclose <- eventM closeBtn Gtk.buttonReleaseEvent
            edelete <- eventM dlg Gtk.deleteEvent
            emsg <- fromAddHandler (addHandler esmsg)

            reactimate $ (handleMsg buffer) <$> emsg
            reactimate $ Gtk.mainQuit <$ eclose
            reactimate $ Gtk.mainQuit <$ edelete
    network <- compile networkDescription
    actuate network

    fire esmsg "Hi!"

    widgetShowAll dlg
    mainGUI

handleMsg :: TextBufferClass self => self -> String -> IO ()
handleMsg buffer msg = do
    textBufferInsertAtCursor buffer msg
    textBufferInsertAtCursor buffer "\n"
