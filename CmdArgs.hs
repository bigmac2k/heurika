{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module CmdArgs (getCmdArgs, Presenter(), Quine(), HeuRika(..), getPrFile, getSpeakerPw, getPrerenderScope, getX, getY, getQuali, getPPdfFile, getPPrerenderScope, getPX, getPY, getPQuali) where

{- Parsing of command line arguments
-}

import System.Console.CmdArgs
import Control.Monad.Error
import System.Random
import System.Directory (canonicalizePath)
import Error

-- password length if password is to be generated
stdPwLen = 20

-- data structure used with metaprogramming magic to generate CmdArgs
data HeuRikaArgs = PresenterArgs {
     prFile :: FilePath
    ,speakerPw :: Maybe String
    ,noSpeaker :: Bool
    ,prerenderScope :: Int
    ,x :: Int
    ,y :: Int
    ,quali :: Int
    }
    | PdfViewerArgs {
     ppdfFile :: FilePath
    ,pprerenderScope :: Int
    ,px :: Int
    ,py :: Int
    ,pquali :: Int
    }
    | QuineArgs deriving (Data, Typeable)

-- Instance used for metaprogramming approach
-- contains descriptions and parameter names
presenter = PresenterArgs {
     prFile = def &= argPos 0
    ,speakerPw = def &= name "p" &= help "Password for speaker access"
    ,noSpeaker = def &= name "n" &= help "Disable speaker functionality - may be ignored by clients"
    ,prerenderScope = def &= name "s" &= help "How many steps to prerender"
    ,x = def &= name "x" &= help "X render size"
    ,y = def &= name "y" &= help "Y render size"
    ,quali = def &= name "q" &= help "Jpeg render quality"
    } &= name "presenter" &= help "Start normal presenter operation"

-- Presenter and Quine are types such i can have record syntax on HeuRikaArgs (needed by th approacH)
-- and at the same time be typesafe
data Presenter = PresenterClean FilePath (Maybe String) Int Int Int Int deriving Show

-- To prevent record update
getPrFile (PresenterClean x _ _ _ _ _) = x
getSpeakerPw (PresenterClean _ x _ _ _ _) = x
getPrerenderScope (PresenterClean _ _ x _ _ _) = x
getX (PresenterClean _ _ _ x _ _) = x
getY (PresenterClean _ _ _ _ x _) = x
getQuali (PresenterClean _ _ _ _ _ x) = x

-- Quine mode
quine = QuineArgs &= name "quine" &= help "Quine mode - beware - possibly overrides files!"

-- Presenter and Quine are types such i can have record syntax on HeuRikaArgs (needed by th approacH)
-- and at the same time be typesafe
-- here if it has to be extended with args...
data Quine = QuineClean deriving Show

pdfViewer = PdfViewerArgs {
     ppdfFile = def &= argPos 0
    ,pprerenderScope = def &= name "s" &= help "How many steps to prerender"
    ,px = def &= name "x" &= help "X render size"
    ,py = def &= name "y" &= help "Y render size"
    ,pquali = def &= name "q" &= help "Jpeg render quality"
    } &= name "pdfviewer" &= help "Start PdfViewer mode"

data PdfViewer = PdfViewerClean FilePath Int Int Int Int deriving Show

getPPdfFile (PdfViewerClean x _ _ _ _) = x
getPPrerenderScope (PdfViewerClean _ x _ _ _) = x
getPX (PdfViewerClean _ _ x _ _) = x
getPY (PdfViewerClean _ _ _ x _) = x
getPQuali (PdfViewerClean _ _ _ _ x) = x

heuRika = cmdArgsMode $ modes [presenter &= auto, quine, pdfViewer] &= help "HeuRika presenter" &= program "HeuRika" &= summary "HeuRika presentation tool"

-- The actual Structure for main
data HeuRika = Presenter Presenter | Quine Quine | PdfViewer PdfViewer deriving Show

getCmdArgs :: (MonadIO m, MonadError HeuError m) => m HeuRika
getCmdArgs = do
    args <- liftIO $ cmdArgsRun heuRika
    case args of
         PresenterArgs prFile' speakerPw' noSpeaker' prerenderScope' x' y' quali' -> liftIO $ do
             prFile'' <- canonicalizePath prFile'
             pw <- case speakerPw' of
                     _ | noSpeaker' -> return Nothing
                     Nothing  -> do
                         pw <- sequence $ replicate stdPwLen $ randomRIO ('a', 'z')
                         putStrLn $ "Generated password: " ++ pw
                         return $ Just pw
                     _ -> return speakerPw'
             return $ Presenter $ PresenterClean prFile'' pw prerenderScope' (cleanSize 1024 x') (cleanSize 768 y') (cleanSize 90 quali')
         QuineArgs -> return $ Quine QuineClean
         PdfViewerArgs pdfFile' prerenderScope' x' y' quali' -> do
             pdfFile'' <- liftIO $ canonicalizePath pdfFile'
             return $ PdfViewer $ PdfViewerClean pdfFile'' prerenderScope' (cleanSize 1024 x') (cleanSize 768 y') (cleanSize 90 quali')
    where cleanSize def x | x <= 0 = def
                          | otherwise = x
