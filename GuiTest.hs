import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad.Trans

main :: IO ()
main = do
    initGUI
    bld <- builderNew
    builderAddFromFile bld "test.glade"
    wnd <- builderGetObject bld castToWindow "main-window"
    wnd `on` deleteEvent $ liftIO onQuit
    nameFld <- builderGetObject bld castToEntry "name-entry" 
    applyBtn <- builderGetObject bld castToButton "apply-button" 
    applyBtn `on` buttonPressEvent $ liftIO $ onApply wnd nameFld
    closeBtn <- builderGetObject bld castToButton "close-button" 
    closeBtn `on` buttonPressEvent $ liftIO onQuit
    widgetShowAll wnd
    mainGUI

onQuit :: IO Bool
onQuit = do
    mainQuit
    return False

onApply :: Window -> Entry -> IO Bool
onApply wnd nameFld = do
    name <- entryGetText nameFld
    let msg = "Hello, " ++ name ++ "!"
    dlg <- messageDialogNew (Just wnd) [ DialogModal ] MessageInfo ButtonsOk msg
    dialogRun dlg
    widgetDestroy dlg
    return False
