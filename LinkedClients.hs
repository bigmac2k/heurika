module LinkedClients (linkedClients) where

{- which clients are linked with Heurika -}

import qualified Types as T

--import qualified Plugins.ImgViewer (client)
import qualified Plugins.GtkViewer (client)
import qualified Plugins.Web (client)

{- This list contains all available clients -}
linkedClients :: [T.Client]
linkedClients = [Plugins.GtkViewer.client, Plugins.Web.client]
