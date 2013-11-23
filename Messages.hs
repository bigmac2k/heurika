module Messages (ImageID, Image(..), NavCommand(..), MToCore(..), MToViewers(..)) where

-- HeuRika message format version 0.00000001 pre-alpha
import Control.Concurrent.STM.TChan(TChan)
import Data.ByteString(ByteString)

{-
 - Message from viewer to core:
 - - update resolution setting
 - - navigation message (up, down, left, right; pop stack; back)
 - - request specific image
 - - (invalid keystroke)?
 -}
type ImageID = Int -- preliminary
data Image = PNG  ByteString
           | JPEG ByteString deriving Show
           -- | ... -- preliminary

data NavCommand = Down | Right | Up | Left | Back | StackBack deriving (Show, Eq, Ord)

data MToCore = UpdateSetting
             | Navigation NavCommand
             | GetImage ImageID (TChan Image)
             | ShutdownReq
             -- | InvalidCommand

instance Show MToCore where
    show UpdateSetting = "UpdateSetting"
    show (Navigation n) = "Navigation " ++ show n ++ ""
    show (GetImage id _) = "GetImage " ++ show id ++ " chan"
    show ShutdownReq = "ShutdownReq"

{-
 - Message from core to viewers (broadcast)
 - - update image (image, terminals)
 - - (invalid keystroke)?
 -}
data MToViewers = UpdateImage Image [NavCommand] ImageID
                | Shutdown
                -- | InvalidCommand

instance Show MToViewers where
    show (UpdateImage _ navs index) = "UpdateImage <imgdata> " ++ show navs ++ show index
    show Shutdown = "Shutdown"
