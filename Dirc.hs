{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Network.Socket
import System.IO

import Graphics.UI.Gtk as Gtk hiding (Event)
import Graphics.UI.Gtk.Builder
import Reactive.Banana
import Reactive.Banana.Gtk

main :: IO ()
main = do
    initGUI
    bld <- builderNew
    builderAddFromFile bld "dirc.glade"
    dlg <- builderGetObject bld castToWindow "main-dialog"
    closeBtn <- builderGetObject bld castToButton "close-button"
    msgTxt <- builderGetObject bld castToTextView "message-text"
    buffer <- textViewGetBuffer msgTxt
    textBufferInsertAtCursor buffer "Hi\n"

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eclose <- eventM closeBtn Gtk.buttonReleaseEvent
            edelete <- eventM dlg Gtk.deleteEvent

            reactimate $ Gtk.mainQuit <$ eclose
            reactimate $ Gtk.mainQuit <$ edelete
    network <- compile networkDescription
    actuate network

    widgetShowAll dlg
    mainGUI
