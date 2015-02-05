{-# LANGUAGE OverloadedStrings #-}

 module Plugins.Web (client) where

import qualified Messages as M
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Base64 as BA
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Text as T
import System.Locale
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status304,status404,status401, parseQuery, Status, ResponseHeaders)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import Data.Maybe
import Data.List
import Data.String.Utils
import Data.Time.Clock
import Data.Time.Format
import Text.JSON(decode,resultToEither,JSValue(JSObject),fromJSObject,readJSON,JSON,showJSON, JSValue(JSNull),encode,toJSObject,Result)
import Control.Concurrent.STM
import Control.Concurrent(newChan,Chan,dupChan,newMVar,forkIO,modifyMVar_,threadDelay,readChan,writeChan)
import Control.Monad.State.Strict
import StaticFiles(staticFiles)

type Ascii = B.ByteString

-- WebClient timeout 
timeOut :: Int
timeOut = 15 * 1000000

--Message format for the master channel
data DataMessage = MESSAGE M.MToViewers | PING | CACHE DataMessage

-- sessionstore - cookie key, remote ip, loggedin, nonce
type SessionStore = [(String, String, Bool, String)]

instance JSON DataMessage where
  readJSON JSNull = return PING
  showJSON PING = JSNull
  showJSON (MESSAGE (M.UpdateImage (M.PNG image) navCommands num)) = JSObject $ toJSObject $ encodeUpdateImage "png" image navCommands num
  showJSON (MESSAGE (M.UpdateImage (M.JPEG image) navCommands num)) = JSObject $ toJSObject $ encodeUpdateImage "jpg" image navCommands num
  showJSON (CACHE (MESSAGE (M.UpdateImage _ navCommands num))) = JSObject $ toJSObject $ encodeUpdateImage "" "" navCommands num
decJSON:: JSON a => [Char] -> [([Char], JSValue)] -> Text.JSON.Result a
decJSON name as = maybe (fail $ "could not find " ++ name ++ "!") readJSON $ lookup name as

--Encode the current image and nav commands as json
encodeUpdateImage:: String -> BU.ByteString -> [M.NavCommand] -> Int -> [(String, JSValue)]
encodeUpdateImage typ image navCommands num = (map (\x->(fst x, showJSON (elem (snd x) navCommands))) [("up", M.Up),("down", M.Down),("left", M.Left), ("right", M.Right), ("stackback", M.StackBack), ("back", M.Back)]) ++ [("id", showJSON num), ("imagetype", showJSON typ), ("image", showJSON (BA.encode image))]

--Initialisation of the master channel for the client output. It caches the current slide and handle the long polling
createMasterChan :: TChan M.MToViewers -> TMVar DataMessage -> IO (TChan DataMessage)
createMasterChan mToViewers seqMessage = do
    tchan <- atomically newTChan :: IO (TChan DataMessage)
    forkIO $ forever $ do
        threadDelay timeOut
        atomically $ writeTChan tchan PING
    forkIO $ forever $ do
        mess <- liftIO $ atomically $ readTChan mToViewers
        liftIO $ atomically $ do
          takeTMVar seqMessage
          putTMVar seqMessage (MESSAGE mess)
          writeTChan tchan (MESSAGE mess)
    return tchan 

--Parsing function for arguments from the core
parseArgs args = if aLen /= 1 then Nothing else case reads (head args) of { [(res, "")] -> Just res; _ -> Nothing }
    where aLen = length args

--The main function and initialisation
client = ("web", start)
start (sprOnly, optPw) args (mToViewers, mToCore) = do
    let authUser = "speaker"
        authPassword = optPw -- Nothing means no speaker track
        authPublic = sprOnly
        port = fromMaybe 3080 $ parseArgs args
    putStrLn $ "Listening on port " ++ show port
    seqMessage <- atomically $ newTMVar PING
    sessionStore <- atomically $ newTMVar ([]::SessionStore)
    masterChan <- createMasterChan mToViewers seqMessage
    run port $ app masterChan mToCore seqMessage sessionStore authUser authPassword authPublic port

--The web application
app :: TChan DataMessage -> TChan M.MToCore -> TMVar DataMessage -> TMVar SessionStore -> String -> Maybe String -> Bool -> Int -> Application
app masterChan mToCore seqMessage sessionStore authUser authPassword authPublic port req respond = do
    s <- liftIO $ atomically $ readTMVar sessionStore
    let id = (getParameter "id" req)
    let cache = (getParameter "cache" req)
    let jsonp = (getParameter "jsonp_callback" req)
    let ip = (takeWhile (/=':') (show (remoteHost req)))
    let cmd = (getParameter "cmd" req)
    let pRole = (getParameter "role" req)
    resp <- case pathInfo req of
        ["update"] -> update masterChan seqMessage id cache jsonp authPublic sessionStore (getCookieKey port req) ip
        ["command"] -> command sessionStore mToCore cmd jsonp (getCookieKey port req) ip
        ["role"] -> role sessionStore jsonp pRole (getCookieKey port req) ip authPublic authPassword
        ["speaker"] -> mainPage "speaker" sessionStore authUser authPassword authPublic port req
        [] ->  mainPage "listener" sessionStore authUser authPassword authPublic port req
        x -> return $ buildResponseFile $ intercalate "/" $ ["Plugins/Web"] ++ (map T.unpack x)
    respond resp


--Return error to the client, that no password and no public track is specified
mainPage :: (MonadIO m) =>  String -> TMVar SessionStore -> String -> Maybe String -> Bool -> Int -> Request -> m Response
mainPage "speaker" sessionStore authUser Nothing authPublic port req = do
  if authPublic then do
    return $ responseBuilder status401 [ ("Content-Type", "text/txt; charset=utf-8")] $ mconcat $ map copyByteString ["No public tracks and no password specified!"]
  else do
    mainPage "listener" sessionStore authUser Nothing authPublic port req

--Return the main page in speaker mode to the client
mainPage "speaker" sessionStore authUser (Just authPassword) authPublic port req = do
  let remote = (takeWhile (/=':') (show (remoteHost req)))
  let hauth = show $ fromMaybe "" $ lookup "Authorization" $requestHeaders req
  let realm = "Rika Presentation App"::String
  let opaque = toMD5 "rika_presenter_app"::String
  store <- liftIO $ atomically $ readTMVar sessionStore
  let nonce = getSessionNonce store (getCookieKey port req) remote
  let c = map (\y->(y !! 0, y !! 1)) $filter (\x-> length x > 1) $ map ((\x-> split "=" x).(\x->  replace " " "" x).(\x->  replace "\"" "" x).(\x-> replace "\\" "" x)) $ split "," hauth
  let clientNC = fromMaybe "" (lookup "nc" c)
  let clientNonce = fromMaybe "" (lookup "cnonce" c)
  let clientResponse = fromMaybe "" (lookup "response" c)
  let ha1 = toMD5 $ authUser ++ ":" ++ realm ++ ":" ++ authPassword
  let ha2 = toMD5 ("GET:/" ++ (intercalate "/" $ map T.unpack $ pathInfo req))
  let response = toMD5 $ ha1 ++":" ++ nonce ++ ":" ++ clientNC ++ ":" ++ clientNonce ++ ":auth:" ++ ha2
  newNonce <- liftIO $ getNewNonce remote
  if (checkSession store (getCookieKey port req) remote) then do
    return $ responseFileLinked status200 [("Content-Type", "text/html; charset=utf-8")] "Plugins/Web/interface.html"
  else if (response == clientResponse) then do
    liftIO $ atomically $ do
      s <- takeTMVar sessionStore
      putTMVar sessionStore $ activateSession s (getCookieKey port req) remote 
    return $ responseFileLinked status200 [ ("Content-Type", "text/html; charset=utf-8")] "Plugins/Web/interface.html"
  else do
    k <- liftIO $ getNewCookie remote
    let s = "Digest realm=\""++ realm ++ "\", qop=\"auth\", nonce=\"" ++ newNonce ++ "\", opaque=\"" ++ opaque ++ "\""
    store <- liftIO $ atomically $ readTMVar sessionStore
    if sessionExists store (getCookieKey port req) remote then do
      let s = "Digest realm=\""++ realm ++ "\", qop=\"auth\", nonce=\"" ++ nonce ++ "\", opaque=\"" ++ opaque ++ "\""
      return $ responseBuilder status401 [ ("Content-Type", "text/txt; charset=utf-8"),( "WWW-Authenticate", BU.fromString s)] $ mconcat $ map copyByteString ["Not authentified!"]
    else do
      k <- liftIO $ getNewCookie remote
      let s = "Digest realm=\""++ realm ++ "\", qop=\"auth\", nonce=\"" ++ newNonce ++ "\", opaque=\"" ++ opaque ++ "\""
      liftIO $ atomically $ do 
        e <- takeTMVar sessionStore
        putTMVar sessionStore $ addSession e k remote newNonce
      return $ responseBuilder status401 [ ("Content-Type", "text/txt; charset=utf-8"),( "WWW-Authenticate", BU.fromString s), ("Set-Cookie",BC.pack("warpsession"++ show port ++"="++ (show k) ++ ";Max-Age=7200;Version=\"1\";Path=/"))] $ mconcat $ map copyByteString ["Not authentified!"]

--Return the main page in listener mode to the client
mainPage "listener" sessionStore authUser authPassword authPublic port req = do
  if authPublic then do
    mainPage "speaker" sessionStore authUser authPassword authPublic port req
  else do
    liftIO $ atomically $ do
      s <- takeTMVar sessionStore
      putTMVar sessionStore $ removeSession s (show $ getCookieKey port req)
    return $ responseFileLinked status200 [ ("Content-Type", "text/html; charset=utf-8"), ("Set-Cookie",BC.pack("warpsession"++ show port ++"=deleted;Max-Age=10;Version=\"1\";  Path=/"))] "Plugins/Web/interface.html"

--Return the current role and the available roles to the client
role :: (MonadIO m) => TMVar SessionStore -> B.ByteString -> B.ByteString -> String -> String -> Bool -> Maybe String -> m Response
role sessionStore jsonpara _ cookie remote authPublic authPassword = do
    store <- liftIO $ atomically $ readTMVar sessionStore
    if (checkSession store (cookie) remote) then
      return $ buildResponse "application/json; charset=utf-8" [ jsonpara, "(", BU.fromString $ Text.JSON.encode (JSObject ( toJSObject [("role", showJSON ("speaker"::B.ByteString)), ("possible", showJSON (roles authPublic authPassword))])) , ");" ] 
    else
      return $ buildResponse "application/json; charset=utf-8" [ jsonpara, "(", BU.fromString $ Text.JSON.encode (JSObject ( toJSObject [("role", showJSON ("listener"::B.ByteString)), ("possible", showJSON (roles authPublic authPassword))])) , ");" ]

--Combine the possiblities of roles 
roles :: Bool -> Maybe String -> [B.ByteString]
roles authPublic authPassword = (f authPublic) ++ (g authPassword)
  where
    f True = []
    f False = ["listener"::B.ByteString]
    g Nothing =[]
    g _ = ["speaker"::B.ByteString]

--Check the permission of the client and sends the command to the core
command :: (MonadIO m) => TMVar SessionStore -> TChan M.MToCore -> B.ByteString -> B.ByteString -> String -> String -> m Response
command sessionStore mToCore cmd jsonpara cookie remote = do
    let cmds = [("up", M.Up),("down", M.Down),("left", M.Left),("right", M.Right),("stackback", M.StackBack),("back", M.Back)]
    s <- liftIO $ atomically $ readTMVar sessionStore
    if (elem cmd (map (\x-> fst x) cmds)) && checkSession s (cookie) remote then do
      liftIO $ atomically $ writeTChan mToCore $ (M.Navigation (snd.head $ filter (\x->fst x == cmd) cmds))
      return $ buildResponse "application/json; charset=utf-8" [ jsonpara, "(", BU.fromString $ Text.JSON.encode True , ");" ]
    else
      return $ buildResponse "application/json; charset=utf-8" [ jsonpara, "(", BU.fromString $ Text.JSON.encode False , ");" ]

--Return the ImageID of the passed message
getImageID (MESSAGE (M.UpdateImage _ _ num)) = Just num
getImageID PING = Nothing


--Send the new update message to the client
update :: (MonadIO m) => TChan DataMessage -> TMVar DataMessage -> BU.ByteString -> BU.ByteString -> BU.ByteString -> Bool -> TMVar SessionStore -> String -> String -> m Response
update masterChan seqMessage id cache jsonpara authPublic sessionStore cookie remote = do
  s <- liftIO $ atomically $ readTMVar sessionStore
  if (not authPublic) || checkSession s (cookie) remote then do
    current <- liftIO $ atomically $ readTMVar seqMessage 
    let n = getImageID current
    if (BU.toString id) == show (fromMaybe 0 n) then
      do
          newChan <- liftIO $ atomically $ dupTChan masterChan
          mess <- liftIO $ atomically $ readTChan newChan
          case mess of
            (MESSAGE m) -> do
              if elem (show (fromMaybe 0 (getImageID (MESSAGE m)))) (split "|" (BU.toString cache)) then do
                return $ resp (CACHE (MESSAGE m))
              else do  
                return $ resp (MESSAGE m)
            m -> do return $ resp m
    else
        do
          if elem (show (fromMaybe 0 n)) (split "|" (BU.toString cache)) then
            case current of
              x -> do  return $ resp (CACHE x)
          else
            case current of
              x -> do  return $ resp x
  else do
    return $ responseBuilder status401 [ ("Content-Type", "text/txt; charset=utf-8")] $ mconcat $ map copyByteString ["Not authentified!"]    
    where
      resp x = buildResponse "application/json; charset=utf-8" [ jsonpara, "(", BU.fromString $ Text.JSON.encode x ,");" ]

--like ResponseFile but retrieve from staticFile
responseFileLinked :: Status -> ResponseHeaders -> FilePath -> Response
responseFileLinked status headers fn = case Map.lookup fn staticFiles of
                                            Nothing -> responseBuilder status404 [("Content-Type", "text/plain; charset=utf-8")] $ mconcat $ map copyByteString ["Error 404"]
                                            Just bs -> responseLBS status headers (BL.fromChunks [bs])

--Send the file with the response to the client
buildResponseFile :: FilePath -> Response
buildResponseFile file | checkTyp ".html" file = responseFileLinked status200 [ ("Content-Type", "text/html; charset=utf-8")] file
                       | checkTyp ".css"  file = responseFileLinked status200 [ ("Content-Type", "text/css; charset=utf-8")] file
                       | checkTyp ".js"   file = responseFileLinked status200 [ ("Content-Type", "text/javascript; charset=utf-8")] file
                       | checkTyp ".png"   file = responseFileLinked status200 [ ("Content-Type", "image/png")] file
                       | checkTyp ".jpg"   file = responseFileLinked status200 [ ("Content-Type", "image/jpeg")] file
                       | otherwise = responseBuilder status404 [("Content-Type", "text/plain; charset=utf-8")] $ mconcat $ map copyByteString ["Error 404"]
  where
    checkTyp typ filepath = ((reverse typ) == (take (length typ) $ reverse filepath))



--Send a text response to the client with a specific tyype
buildResponse:: Ascii -> [BU.ByteString] -> Response
buildResponse typ content = responseBuilder status200 [("Content-Type", typ)] $ mconcat $ map copyByteString content

--Get a parameter value of the uri with a specific name
getParameter :: BU.ByteString -> Request -> BU.ByteString
getParameter x req =  getValue $ filter(\y->fst y == x) $ parseQuery $ rawQueryString req
  where
    getValue :: [(BU.ByteString, Maybe BU.ByteString)] -> BU.ByteString
    getValue [] = ""
    getValue ((_, Nothing):_) = ""
    getValue ((_, Just x):_) = x

--Return the actually time value as string.
getCurTime :: IO String
getCurTime = do
  now <- getCurrentTime
  return (formatTime defaultTimeLocale "%T" now)

--Generate nonce with current time, remote address and constant strings.
getNewNonce :: String -> IO String
getNewNonce remote = do
  time <- getCurTime
  return $ SHA.showDigest $  SHA.sha256 $ BL.pack $ "NONCE" ++ time ++ remote ++ "APP"

--Generates new Cookie with the hash of the current time and remote address.
getNewCookie :: String -> IO String
getNewCookie remote = do
  time <- getCurTime
  return $ SHA.showDigest $  SHA.sha256 $ BL.pack $ time ++ remote
  
--Check whether the session exists and the access is allowed. Therefore the cookie value, remote address and the boolean, whether the access it granded will be checked.
checkSession :: SessionStore -> String -> String -> Bool
checkSession [] cookie remote = False
checkSession ((c,r,b,_):xs) cookie remote | b == True && c==cookie && r==remote = True
                                          | otherwise = checkSession xs cookie remote

--Checks whether a session of the remote exists already. Therefore it look the cookie value and the remote address up.
sessionExists :: SessionStore -> String -> String -> Bool
sessionExists [] _ _ = False
sessionExists ((c,r,b,n):xs) cookie remote | c==cookie && r==remote = True
                                           | otherwise = sessionExists xs cookie remote

--Activate a session with help of the remote address and the cookie value.
activateSession :: SessionStore -> String -> String -> SessionStore
activateSession store cookie remote = (filter (\(x,y,_,_) -> x/=cookie || y /=remote) store) ++ (map (\(x,y,_,n)->(x,y,True,n)) $ (filter (\(x,y,_,n) -> x==cookie && y==remote) store))

--Get the nonce value of a cookie with remote address check.
getSessionNonce :: SessionStore -> String -> String -> String
getSessionNonce [] cookie remote = ""
getSessionNonce ((c,r,b,n):xs) cookie remote | c==cookie && r==remote = n
                                             | otherwise = getSessionNonce xs cookie remote

--Remove a session from the session storage.
removeSession :: SessionStore -> String -> SessionStore
removeSession store cookie = filter (\(x,y,_,_) -> x/=cookie) store

--Add a new session to the session storage.
addSession :: SessionStore -> String -> String -> String -> SessionStore
addSession store cookie remote nonce = store ++ [(cookie,remote, False,nonce)]

--Get the Cookie value of the request
getCookieKey :: Int -> Request -> String
getCookieKey port req = f $ requestHeaders req
  where
    f [] = ""
    f (("Cookie", x):xs) = g port ((\x -> split ";" x) (filter (\y -> y /= ' ' && y /='"') (show x)))
--    f (("Cookie", x):xs)  | (take (length $ "warpsession" ++ show port ++ "=") (filter (\x -> x /='\\' && x /='"') $ show x) == ("warpsession" ++ show port ++ "=")) = drop (length $ "warpsession" ++ show port ++ "=") $ filter (\x -> x /='\\' && x /='"') $ show x
    f (_:xs) = f xs
    g _ [] = ""
    g port (x:xs) | (take (length $ "warpsession" ++ show port ++ "=") (filter (\x -> x /='\\' && x /='"') $ show x) == ("warpsession" ++ show port ++ "=")) = drop (length $ "warpsession" ++ show port ++ "=") $ filter (\x -> x /='\\' && x /='"') $ show x
                  | otherwise = g port xs

--Hash a string and return the result as string
toMD5 :: String -> String
toMD5 s = toHex $ BC.unpack $ MD5.hash $ BC.pack s

--Transform the string to hex and return it as string
toHex :: String -> String
toHex = concatMap (\word -> let index = fromEnum word in [alpha !! (index `div` 16), alpha !! (index `mod` 16)])
    where alpha = ['0' .. '9'] ++ ['a' .. 'f']
