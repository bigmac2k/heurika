module Main where

{- HeuRika, the magnificant presentation tool... -}

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Array as A
import Control.Monad.Error
import Control.Monad
import Data.Maybe
import System.IO (getChar)

import Graphics.UI.Gtk (initGUI, mainGUI)

import CmdArgs
import PdfGraph
import Parser
import Error
import Renderer
import Core
import qualified Types as T
import qualified Messages as Mess
import qualified StaticFiles

import LinkedClients

{- start clients
 - TODO(SCM): wrap to proper errorT?
 - What can possibly go wrong? :P
 -}
-- start the clients. They need to be linked in LinkedClients
startClients :: (MonadIO m) => Maybe String -> [Int] -> [[Screen]] -> m ([TChan Mess.MToViewers], TChan Mess.MToCore)
startClients speakerPw speakerTracks screenss = liftIO $ do
    fromClientChan <- atomically newTChan
    toClientChans <- forM (zip [1..] screenss) $ \(i, screens) -> do
        let speakerOnly = i `elem` speakerTracks
        toClientChan <- atomically newTChan
        forM_ screens $ \(Screen name args) -> do
            let client = fromJust $ lookup name linkedClients
            clChan <- atomically $ dupTChan toClientChan
            forkIO $ client (speakerOnly, speakerPw) args (clChan, fromClientChan)
        return toClientChan
    return (toClientChans, fromClientChan)

-- main entry point
main :: IO ()
main = runErrorTReport $ do
    liftIO $ do
        initGUI
        forkOS mainGUI
    mode <- getCmdArgs
    let availableClients = map fst linkedClients
    case mode of
         Presenter config -> do
             (header, nodes, edges, screens) <- getPresentationConfig availableClients (getPrFile config)
             let graph = constructGraph nodes edges -- TODO(SCM): add checks and transform constructGraph to error monad

             (toClientChans, fromClientChan) <- startClients (getSpeakerPw config) (tracks header) screens
             (worklist, cache) <- startRenderer (getQuali config) (getX config) (getY config) (pdfFile header)

             liftIO $ startCore fromClientChan toClientChans worklist cache graph (startNode header) (getPrerenderScope config)
         Quine _ -> liftIO $ do
             putStrLn "I have to barf!\nAll files out of the way if you do not want to be colleteral!\n\nEverybody ready?"
             getChar
             StaticFiles.unpack
         PdfViewer config -> do
             (header, nodes, edges, screens) <- getViewerPresentationConfig availableClients (getPPdfFile config)
             let graph = constructGraph nodes edges -- TODO(SCM): add checks and transform constructGraph to error monad

             (toClientChans, fromClientChan) <- startClients Nothing (tracks header) screens
             (worklist, cache) <- startRenderer (getPQuali config) (getPX config) (getPY config) (pdfFile header)

             liftIO $ startCore fromClientChan toClientChans worklist cache graph (startNode header) (getPPrerenderScope config)
