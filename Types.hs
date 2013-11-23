module Types where

{- Some shared types other than messages -}

import Messages
import Control.Concurrent.STM.TChan(TChan)

type Client = (String, (Bool, Maybe String) -> [String] -> (TChan MToViewers, TChan MToCore) -> IO ())

data Groups = All | Client | Responder deriving (Show, Ord, Eq)
