{-# LANGUAGE FlexibleContexts #-}
module Renderer (startRenderer,openPdfFile) where

{- HeuRikas renderer
- uses Poppler and Cairo to render a pdf slide to
- an array of Word8, and then some magic and a jpg converter
-}

import Control.Concurrent.STM
import Control.Monad.Error
import Control.Concurrent (forkIO)
import qualified Data.Array as A
import Data.Array.IArray
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.IO
import Codec.Picture(PixelRGB8(PixelRGB8),generateImage,DynamicImage(ImageRGB8))
import Codec.Picture.Saving
import Data.Word
import Data.Bits

import Graphics.Rendering.Cairo
--import Graphics.UI.Gtk (postGUISync) -- just be sure
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page

import Error
import qualified Messages as Mess

postGUISync = id

initCache :: Int -> IO (A.Array Int (TMVar a))
initCache numPages = do
    tmvars <- sequence $ replicate numPages newEmptyTMVarIO
    return $ A.array (1, numPages) $ zip [1..] tmvars

openPdfFile :: (MonadIO m, MonadError HeuError m) => FilePath -> m Document
openPdfFile fn = do
    doc <- liftIO $ postGUISync $ documentNewFromFile ("file://" ++ fn) Nothing
    maybe (throwError $ FileError $ "Could not open " ++ fn) return doc

getWlItem :: TMVar [[a]] -> IO a
getWlItem wl = atomically $ do
    ((item : itemrest) : wlrest) <- takeTMVar wl
    let wl' = if null itemrest then wlrest else itemrest : wlrest
    when (not $ null wl') $ putTMVar wl wl'
    return item

startRenderer :: (MonadIO m, MonadError HeuError m) => Int -> Int -> Int -> FilePath -> m (TMVar [[Int]], Array Int (TMVar Mess.Image))
startRenderer jpegQuali xrender yrender fn = do
    doc <- openPdfFile fn
    liftIO $ do
        numPages <- postGUISync $ documentGetNPages doc
        cache <- initCache numPages
        worklist <- newEmptyTMVarIO
        forkIO $ forever $ do
            item <- getWlItem worklist
            render doc cache item
        return (worklist, cache)
    where
    render :: DocumentClass doc => doc -> Array Int (TMVar Mess.Image) -> Int -> IO ()
    render doc cache id = do
        let mvar = cache A.! id
        isEmpty <- atomically $ isEmptyTMVar mvar
        if isEmpty then putStrLn $ "Rendering id: " ++ show id else putStrLn $ "Already rendered: " ++ show id
        when isEmpty $ do
            page <- postGUISync $ documentGetPage doc $ pred id
            (x, y) <- postGUISync $ pageGetSize page
            bs <- renderToImage x y $ pageRender page
            success <- atomically $ tryPutTMVar mvar $ Mess.JPEG bs
            when (not success) $ hPutStrLn stderr "omitted writing - someone else did"
        where
        renderToImage :: Double -> Double -> Render a -> IO BS.ByteString
        renderToImage x y render = do
            (pixelData, stride) <- withImageSurface FormatRGB24 ix iy $ \surface -> do
                stride <- imageSurfaceGetStride surface
                renderWith surface $ do
                    setSourceRGB 1 1 1
                    paint
                    scale scaleValue scaleValue
                    render
                pData <- imageSurfaceGetPixels surface >>= freeze :: IO (UArray Int Word8)
                return (pData, stride)
            let image = {-# SCC "scc_image" #-} ImageRGB8 $ {-# SCC "scc_generateImage" #-} generateImage (dataToPixel pixelData stride) ix iy
                jpg = {-# SCC "scc_jpg" #-} LBS.toStrict $ imageToJpg jpegQuali image
            jpg `seq` return jpg
            where
            scaleValue = min (fromIntegral xrender / x) (fromIntegral yrender / y)
            ix = ceiling (scaleValue * x)
            iy = ceiling (scaleValue * y)
            -- beware: problematic with endianness
            -- use old method (w32tow8s) if necessary
            dataToPixel pixelData stride x' y' =
                let i = y' * stride + (x' * 4)
                    b = pixelData ! (i + 0)
                    g = pixelData ! (i + 1)
                    r = pixelData ! (i + 2) in
                    PixelRGB8 r g b
