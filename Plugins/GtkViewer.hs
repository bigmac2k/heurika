module Plugins.GtkViewer (client) where

import Graphics.UI.Gtk
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import System.IO (openTempFile,hClose,hPutStrLn,stderr)
import System.Directory (removeFile)
import qualified Data.Text as Text

import qualified Types as T
import qualified Messages as M

client :: T.Client
client = ("newnative", \_ _ -> start)

start (fromCore, toCore) = do
    updateImage <- postGUISync $ do
        window <- windowNew
        imageView <- imageNew
        containerAdd window imageView
        widgetShowAll window
        window `on` keyPressEvent $ tryEvent $ do
            event <- eventKeyName
            eventHandler event
        {-
         - -- bug here - should resize also when window size changes, not only if new image there
         -window `on` configureEvent $ tryEvent $ do
         -    (width, height) <- eventSize
         -    liftIO $ containerForeach window $ \widget -> when (widget `isA` gTypeImage) $ do
         -      let image = castToImage widget
         -      stype <- image `get` imageStorageType
         -      case stype of
         -        ImagePixbuf -> do
         -          pixbuf <- imageGetPixbuf image
         -          width <- pixbufGetWidth pixbuf
         -          height <- pixbufGetHeight pixbuf
         -          print (width, height)
         -        _ -> return ()
         -}
        return $ \bs -> do
            (tempName, tempHandle) <- openTempFile "." "temp.jpg"
            BS.hPut tempHandle bs
            (width, height) <- windowGetSize window
            pixbuf <- pixbufNewFromFileAtScale tempName width height True
            hClose tempHandle
            removeFile tempName
            postGUIAsync $ imageSetFromPixbuf imageView pixbuf
    forever $ do
        M.UpdateImage (M.JPEG img) _ _ <- atomically $ readTChan fromCore
        updateImage img
    where
    send = liftIO . atomically . writeTChan toCore . M.Navigation
    eventHandler event = maybe (err event) id . lookup event $ map (\(a, b) -> (Text.pack a, b)) $
        [("Down", send M.Down)
        ,("Up", send M.Up)
        ,("Right", send M.Right)
        ,("Left", send M.Left)
        ,("space", send M.Down)
        ,("BackSpace", send M.Back)
        ,("Alt_R", send M.StackBack)
        ]
    err event = liftIO $ hPutStrLn stderr $ "Could not handle event " ++ Text.unpack event
