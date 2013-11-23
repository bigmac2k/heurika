{-# LANGAUGE ForeignFunctionInterface #-}
 module Plugins.Sdl (start) where
import Types
import Messages

import Foreign
import Foreign.C.Types
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types (RWopsStruct,RWops,surfaceGetHeight,surfaceGetWidth)
import Graphics.UI.SDL.RWOps (mkFinalizedRW)
import qualified Graphics.UI.SDL.Image as SDL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as IBS (toForeignPtr)
import qualified Graphics.GD.ByteString as GD
--import qualified Graphics.Exif as E
import qualified System.CPUTime as C
import System.Environment
import Control.Applicative
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Color
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad

withBSPtr :: Integral len => (Ptr a -> len -> IO b) -> BS.ByteString -> IO b
withBSPtr f bs = let (fPointer, fOffset, fLength) = IBS.toForeignPtr bs in
    withForeignPtr fPointer $ \ptr -> f (plusPtr ptr $ fromIntegral fOffset) (fromIntegral fLength)

foreign import ccall unsafe "SDL_RWFromMem" c_rwFromMem :: Ptr Word8 -> CInt -> IO (Ptr RWopsStruct)

rwFromBS :: BS.ByteString -> IO RWops
rwFromBS = (>>= mkFinalizedRW) . withBSPtr c_rwFromMem

--exifFromBS :: BS.ByteString -> IO E.Exif
--exifFromBS = withBSPtr $ E.fromData . castPtr

prepareImage screenFormat bs = do
    liftGD bs $ \image -> do
        format <- GD.imageSize image
        let (width, height) = scale format screenFormat
        GD.resizeImage width height image
    where
    scale :: (Int, Int) -> (Int, Int) -> (Int, Int)
    scale (srcWidth, srcHeight) (tWidth, tHeight) = let rSrcWidth = fromIntegral srcWidth
                                                        rSrcHeight = fromIntegral srcHeight
                                                        rTWidth = fromIntegral tWidth
                                                        rTHeight = fromIntegral tHeight
                                                        (rWidth, rHeight) = (rSrcWidth / rTWidth, rSrcHeight / rTHeight)
                                                        rMax = max rWidth rHeight :: Rational
                                                    in (truncate $ rSrcWidth / rMax, truncate $ rSrcHeight / rMax)

liftGD :: BS.ByteString -> (GD.Image -> IO GD.Image) -> IO BS.ByteString
liftGD bs f = GD.loadJpegByteString bs >>= f >>= GD.saveJpegByteString 100

quitHandler :: IO ()
quitHandler = do
    e <- SDL.waitEvent
    case e of
        SDL.Quit -> return ()
        SDL.KeyDown (SDL.Keysym SDL.SDLK_q [] _) -> return ()
        _ -> print e >> quitHandler

start :: Client
start (mToViewers, mToCore) = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 0 0 32 [] -- [SDL.Fullscreen]
    forever $ do
        surface <- SDL.getVideoSurface
        SDL.fillRect surface Nothing $ Pixel $ minBound
        UpdateImage (JPEG bs) _ <- atomically $ readTChan mToViewers
        bs' <- prepareImage (surfaceGetWidth surface, surfaceGetHeight surface) bs
        image <- rwFromBS bs' >>= flip SDL.loadRW False
        SDL.blitSurface image Nothing surface Nothing
        SDL.flip surface

{-
main :: IO ()
main = do
    fn <- head <$> getArgs
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 0 0 32 [SDL.Fullscreen]
    surface <- SDL.getVideoSurface
    SDL.fillRect surface Nothing $ Pixel $ minBound
    bs <- BS.readFile fn
    --exifFromBS bs >>= E.allTags >>= print
    --start <- C.getCPUTime
    --bs <- liftGD bs $ (>>= GD.rotateImage 1) . GD.resizeImage 768 512
    bs <- prepareImage (surfaceGetWidth surface, surfaceGetHeight surface) bs
    --C.getCPUTime >>= print . (\stop -> stop - start)
    image <- rwFromBS bs >>= flip SDL.loadRW False
    SDL.blitSurface image Nothing surface Nothing
    SDL.flip surface
    quitHandler
-}

