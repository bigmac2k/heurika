module Plugins.ImgViewer (client, start) where  

import Control.Monad.IO.Class
import Data.Char
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.PixbufAnimation
import System.IO
import System.Environment
import System.Directory
--import Text.Regex.Posix
import Control.Concurrent


import qualified Messages as M
import qualified Types as T
import qualified Data.ByteString as B

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)


import Control.Concurrent.STM
import Control.Monad



--main :: t -> (t1, t2) -> IO ()
client = ("native", \_ _ -> start)
start (fromCore, toCore) = do
  timeoutAdd (yield >> return True) 50      --allow other threads to work
  Just xml    <- xmlNew "Plugins/ImgViewer/Layout.glade"
  window      <- xmlGetWidget xml castToWindow    "window1"             
  image       <- xmlGetWidget xml castToImage     "image1"    --include an image in the view
  windowSetDefaultSize window 400 300
  windowSetPosition window WinPosCenter
  windowSetTitle window "HeuRika"
  fullscreen  <- newIORef False             --window is not in fullscreen in the beginning        
  posNavs      <- newIORef []
  (f,x) <- (openTempFile "." "foo.jpg") --generating a tmp file and use it through out the program
  fName       <- newIORef f             --save the filename
  -- space       <- newIORef $ (400,300)
  hClose x                              --close the handler so the file can be used
  on window objectDestroy $ do          --close the window and delete the tmp file
    f <- readIORef fName 
    removeFile f
    putStrLn $ "killed"
    mainQuit
  on window keyPressEvent $ tryEvent $ do
    x <- eventKeyName
    liftIO $ do
      pNavs <- readIORef posNavs
      case x of
        "q" -> liftIO $ do                        --send kill to core
          let toSend = M.ShutdownReq
          atomically $ writeTChan toCore toSend
        "space" ->  liftIO $ do                   --if space is pressed enter
          if elem M.Down pNavs                    --check if down is availble for the current image
            then do
              let toSend = M.Navigation M.Down    --set the down command as the one we want to send
              atomically $ writeTChan toCore toSend --write the down command in the channel to the core, without being interrupted
          else
            putStrLn $ "Down not allowed"
        "Escape" -> liftIO $ do                   --leave fullscreen
          fs <- readIORef fullscreen
          if fs then do                           --only enter when we are in fullscreen mode
            writeIORef fullscreen False           --reset fullscreen boolean
            windowUnfullscreen window             --leave fullscreen mode
            threadDelay 500000                    --thread delay between leaving the fullscreen and resizing the image, since we resize based on the availible space and the window needs some time to actually resize 
            f <- readIORef fName                  
            availableSpace <- widgetGetSize window
            rescaleImage image availableSpace f   --resize the image based on the window size
          else putStrLn ""
        "f" -> liftIO $ do                        --enter fullscreen, rather similar to leaving it
          writeIORef fullscreen True
          windowFullscreen window
          threadDelay 500000
          f <- readIORef fName
          availableSpace <- widgetGetSize window
          rescaleImage image availableSpace f
        "Down" -> liftIO $ do                     --same as for the space case. The next ones only change the key being pressed and the command being checked and send.
          if elem M.Down pNavs
            then do
              let toSend = M.Navigation M.Down
              atomically $ writeTChan toCore toSend
          else
            putStrLn $ "Down not allowed"
        "Up" -> liftIO $ do
          if elem M.Up pNavs 
            then do
              let toSend = M.Navigation M.Up
              atomically $ writeTChan toCore toSend
          else
            putStrLn $ "Up not allowed"
        "Left" -> liftIO $ do
          if elem M.Left pNavs 
            then do
              let toSend = M.Navigation M.Left
              atomically $ writeTChan toCore toSend
          else
            putStrLn $ "Left not allowed"
        "Right" -> liftIO $ do
          if elem M.Right pNavs
            then do
              let toSend = M.Navigation M.Right
              atomically $ writeTChan toCore toSend
          else
            putStrLn $ "Right not allowed"
        "BackSpace" -> liftIO $ do
          if elem M.Back pNavs
            then do
              let toSend = M.Navigation M.Back
              atomically $ writeTChan toCore toSend
          else
            putStrLn $ "Back not allowed"
        "Alt_R" -> liftIO $ do
          if elem M.StackBack pNavs
            then do
              let toSend = M.Navigation M.StackBack
              atomically $ writeTChan toCore toSend
          else
            putStrLn $ "StackBack not allowed"
  --on window windowBeginResizeDrag $ tryEvent $ do
    --putStrLn $ "yay"
  
  -- on window configureEvent $ tryEvent $ do
  --         (w, h) <- eventSize
  --         liftIO $ do
  --              (w2, h2) <- readIORef space   
  --              if (w == w2) && (h == h2) 
  --                then do 
  --                  putStrLn $ "nothing changed" ++ (show w) ++ (show h) ++ " " ++ (show w2) ++ (show h2)
  --              else do
  --                writeIORef space (w,h)
  --                putStrLn $ "yay" ++ (show w) ++ (show h)
  widgetShowAll window
  msg@(M.UpdateImage x navs _) <- atomically $ readTChan fromCore   --read the initial msg from the core
  writeIORef posNavs navs     --save the possible navigations
  liftIO $ do 
    f <- readIORef fName
    --(w2, h2) <- readIORef space
    --putStrLn $ "ClientRecv"  ++ ": " ++ show w2 ++ show h2   
    imageSetAndScale image x f      --add the image and scale it
  liftIO $ forever $ do             --include the reading from the core channel into the mainIteration
                mainIterationDo False
                threadDelay 1000
                mmsg <- atomically $ tryReadTChan fromCore  --see if there is anything in the channel from the core and act based upon it
                case mmsg of
                  Nothing -> return ()
                  Just msg@(M.UpdateImage x navs _) -> do
                    writeIORef posNavs navs               --save the possible navigations
                    liftIO $ do 
                      f <- readIORef fName
                      --(w2, h2) <- readIORef space   
                      --putStrLn $ "ClientRecv"  ++ ": " ++ show w2 ++ show h2
                      --imageSetAndScale image x (w2, h2) f
                      imageSetAndScale image x f
                  Just M.Shutdown -> do
                    putStrLn $ "kill soon"
                    widgetDestroy window
                
  --mainGUI
  
  
rescaleImage ::  Image -> (Int,Int) -> FilePath -> IO ()
rescaleImage image (x,y) f = do
  availableSpace <- widgetGetSize image 
  pix <- pixbufNewFromFileAtScale f (x-2) (y-2) True
  postGUIAsync $ imageSetFromPixbuf image pix
  return ()

    
imageSetAndScale ::  Image  -> M.Image -> FilePath -> IO ()
imageSetAndScale image  (M.JPEG x) f = do
  B.writeFile f x
  availableSpace <- widgetGetSize image 
  pix <- pixbufNewFromFileAtScale f (fst availableSpace) (snd availableSpace) True
  postGUIAsync $ imageSetFromPixbuf image pix
  return ()
