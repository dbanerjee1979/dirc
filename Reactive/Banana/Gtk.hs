{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.Gtk (Prop'(..)
                           , eventM, event0, event1, event2, event3
                           , behavior, sink
                           , module Reactive.Banana.Frameworks
                           ) where

import Control.Monad.Reader
import Foreign.Ptr
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Glib.Attributes
import System.Glib.Signals

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.General.Enums as Gtk

eventM :: Frameworks t => self -> Signal self (Gtk.EventM a Bool) -> Moment t (Event t (Ptr a))
eventM self signal = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ self `on` signal $ Gtk.tryEvent $ ask >>= liftIO . runHandlers
  fromAddHandler addHandler
 
eventN :: Frameworks t => ((a -> IO ()) -> callback) -> self -> Signal self callback -> Moment t (Event t a)
eventN f self signal = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ self `on` signal $ f runHandlers
  fromAddHandler addHandler

event0 :: Frameworks t => self -> Signal self (IO ()) -> Moment t (Event t ())
event0 = eventN ($ ())

event1 :: Frameworks t => self -> Signal self (a -> IO ()) -> Moment t (Event t a)
event1 = eventN id

event2 :: Frameworks t => self -> Signal self (a -> b -> IO ()) -> Moment t (Event t (a, b))
event2 = eventN curry

event3 :: Frameworks t => self -> Signal self (a -> b -> c -> IO ()) -> Moment t (Event t (a, b, c))
event3 = eventN $ \f a b c -> f (a, b, c)

behavior :: Frameworks t => self -> Attr self a -> Moment t (Behavior t a)
behavior widget attr = fromPoll $ get widget attr

data Prop' t w = forall a. (Attr w a) :== Behavior t a

infixr 0 :==

sink :: Frameworks t => self -> [Prop' t self] -> Moment t ()
sink self = mapM_ sink1 
  where
    sink1 (attr :== b) = do
      x <- initial b
      liftIOLater $ set self [attr := x]
      e <- changes b
      reactimate' $ (fmap $ \x -> set self [attr := x]) <$> e

reactimateEventM :: Frameworks t => (Event t (Ptr a)) -> Gtk.EventM a () -> Moment t ()
reactimateEventM event reader = reactimate $ (<$> event) $ runReaderT reader
