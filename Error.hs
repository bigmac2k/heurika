{-# LANGUAGE ScopedTypeVariables #-}
module Error where

{- Wrapper for errors such that we can handle
- exceptional behavior in the Error Monad
- Not shure if this was a terribly good idea though
- I always struggle with handling errors in haskell as there are many ways...
-}

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.Error
import Control.DeepSeq
import qualified Text.Parsec (ParseError())
import System.Exit

import System.IO

data HeuError =
      Generic
    | GenericString String
    | FileError String
    | ParseError Text.Parsec.ParseError
    | CmdArgsError
      deriving Show

instance Error HeuError where
    noMsg = Generic
    strMsg = GenericString

-- catches an unbounded exception and injects it into the error monad
catchToBounded :: (Exception a, MonadIO m, MonadError b1 m) =>IO b -> (a -> b1) -> m b
catchToBounded io handler = do
    res <- liftIO $ (io >>= return . Right) `catch` (return . Left)
    either (throwError . handler) return res

handleToBounded :: (Exception a, MonadIO m, MonadError b1 m) =>(a -> b1) -> IO b -> m b
handleToBounded = flip catchToBounded

runErrorTReport em = do
    res <- runErrorT em
    case res of
         Right r -> return r
         Left err -> liftIO $ do
             hPutStrLn stderr $ show err
             exitFailure

-- Wrapped functions
readFileError fn = do
     readFile fn
     `catchToBounded` (\(e :: IOException) -> FileError $ show e)

readFileError' fn = do
     bracket (openFile fn ReadMode) hClose (\fh -> hGetContents fh >>= \str -> str `deepseq` return str)
     `catchToBounded` (\(e :: IOException) -> FileError $ show e)
