module Core (startCore) where

{- implementation of the core
- it processes messages that it receives from clients
- and spawns (and manages through ActorActor) processes that respond to
- the clients request
- Further more, it manages a worklist for the renderer
-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State.Strict
import qualified Data.Graph.Inductive.Graph as FGL
import System.IO
import qualified Data.Array as A

import qualified History as Hist
import qualified Messages as Mess
import qualified Types as T
import PdfGraph (bfs)

-- start the core
startCore :: FGL.Graph gr =>
       TChan Mess.MToCore
    -> [TChan Mess.MToViewers]
    -> TMVar [[Mess.ImageID]]
    -> A.Array Mess.ImageID (TMVar Mess.Image)
    -> gr [Int] Mess.NavCommand
    -> FGL.Node
    -> Int
    -> IO ()
startCore fromClientsChan toClientsChans worklist cache graph startNode prerenderScope = do
    let context = FGL.context graph startNode
    contextMVar <- newTMVarIO context
    flip runStateT (context, Hist.empty) $ forever $ do
        answerRequest contextMVar
        msg <- liftIO $ atomically $ readTChan fromClientsChan
        case msg of
             Mess.Navigation navCommand -> do
                 --liftIO $ putStrLn $ "Received navcommand: " ++ show navCommand
                 updateState contextMVar navCommand
             Mess.ShutdownReq -> forM_ toClientsChans $ liftIO . atomically . flip writeTChan Mess.Shutdown
             _ -> liftIO $ hPutStrLn stderr $ "Unimplemented message: " ++ show msg
    return ()
    where
    updateState contextMVar navCommand = do
        (cont, hist) <- get
        let updateNormal histAdder = case lookup navCommand $ map (\(_, dst, dir) -> (dir, dst)) $ FGL.out' cont of
             Nothing -> liftIO $ hPutStrLn stderr $ "Got non-available target - ignoring"
             Just dst -> do
                 let newContext = FGL.context graph dst
                 liftIO $ atomically $ takeTMVar contextMVar >> putTMVar contextMVar newContext
                 put (newContext, histAdder (FGL.node' cont) hist)
            updateHist histPopper = case histPopper hist of
             (Nothing, _) -> liftIO $ putStrLn "Empty History"
             (Just node, newHist) -> do
                 let newContext = FGL.context graph node
                 liftIO $ atomically $ takeTMVar contextMVar >> putTMVar contextMVar newContext
                 put (newContext, newHist)
        case navCommand of
             Mess.Down -> updateNormal Hist.addHist
             Mess.Right -> updateNormal Hist.addStack
             Mess.Up -> updateNormal Hist.addHist
             Mess.Left -> updateNormal Hist.addStack
             Mess.Back -> updateHist Hist.histPop
             Mess.StackBack -> updateHist Hist.stackPop
    updateWorklist context = atomically $ do
        let nodesToRender = bfs graph (Just prerenderScope) (FGL.node' context)
            toRender = concatMap FGL.lab' $ map (FGL.context graph) nodesToRender
        empty <- isEmptyTMVar worklist
        if empty then putTMVar worklist [toRender] else do
            wl <- takeTMVar worklist
            putTMVar worklist (toRender : wl)
    -- Answer a request, making shure that this answer is still valid
    answerRequest contextMVar = get >>= \(context, hist) -> do
        liftIO $ do
            updateWorklist context
            let histTargets = (maybe [] (\_ -> [Mess.Back]) $ fst $ Hist.histPop hist)
                      ++ (maybe [] (\_ -> [Mess.StackBack]) $ fst $ Hist.stackPop hist)
            forM_ (zip toClientsChans (FGL.lab' context)) $ \(chan, id) ->
                forkIO $ atomically $ do
                    image <- readTMVar (cache A.! id)
                    let targets = map (\(_, _, x) -> x) (FGL.out' context) ++ histTargets
                        message = Mess.UpdateImage image targets id
                    cont' <- readTMVar contextMVar
                    when (cont' == context) $ writeTChan chan message
