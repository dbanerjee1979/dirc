{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Data.Maybe
import Graphics.UI.Gtk as Gtk hiding (Event) 
import Graphics.UI.Gtk.Builder
import Control.Monad.Trans
import Reactive.Banana
import Reactive.Banana.Gtk

main :: IO () 
main = do
    initGUI
    bld <- builderNew
    builderAddFromFile bld "test.glade"
    wnd <- builderGetObject bld castToWindow "main-window"
    lhsEntry <- builderGetObject bld castToEntry "lhs-entry"
    rhsEntry <- builderGetObject bld castToEntry "rhs-entry"
    ansLabel <- builderGetObject bld castToLabel "answer-label"
    closeBtn <- builderGetObject bld castToButton "close-button" 

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eclose <- eventM closeBtn Gtk.buttonReleaseEvent
            edelete <- eventM wnd Gtk.deleteEvent

            elhs <- event0 lhsEntry Gtk.editableChanged
            blhs <- behavior lhsEntry Gtk.entryText

            erhs <- event0 rhsEntry Gtk.editableChanged
            brhs <- behavior rhsEntry Gtk.entryText

            let result :: Behavior t (Maybe Int)
                result = f <$> blhs <*> brhs
                         where f x y = liftA2 (+) (readNumber x) (readNumber y)
                readNumber s = listToMaybe [x | (x, "") <- reads s]
                showNumber = maybe "!" show
            sink ansLabel [ Gtk.labelLabel :== showNumber <$> result ]

            reactimate $ Gtk.mainQuit <$ eclose
            reactimate $ Gtk.mainQuit <$ edelete
    network <- compile networkDescription
    actuate network 

    widgetShowAll wnd
    mainGUI
