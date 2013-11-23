module Plugins.GtkViewer (client) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ImageView
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import System.IO (openTempFile,hClose)
import System.Directory (removeFile)

import qualified Types as T
import qualified Messages as M

client :: T.Client
client = ("newnative", \_ _ -> start)

start (fromCore, toCore) = do
    updateImage <- postGUISync $ do
        window <- windowNew
        imageView <- imageViewNew
        containerAdd window imageView
        widgetShowAll window
        window `on` keyPressEvent $ tryEvent $ do
            event <- eventKeyName
            eventHandler event
        return $ \bs -> do
            (tempName, tempHandle) <- openTempFile "." "temp.jpg"
            BS.hPut tempHandle bs
            pixbuf <- pixbufNewFromFile tempName
            hClose tempHandle
            removeFile tempName
            postGUIAsync $ imageViewSetPixbuf imageView (Just pixbuf) True
    forever $ do
        M.UpdateImage (M.JPEG img) _ _ <- atomically $ readTChan fromCore
        updateImage img
    where
    send = liftIO . atomically . writeTChan toCore . M.Navigation
    eventHandler event = case event of
        "Down" -> send M.Down
        "Up" -> send M.Up
        "Right" -> send M.Right
        "Left" -> send M.Left
        "space" -> send M.Down
        "BackSpace" -> send M.Back
        "Alt_R" -> send M.StackBack
