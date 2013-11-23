{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser (getPresentationConfig, Header(..), Screen(..), getViewerPresentationConfig) where

{- Parser for the presentation config file -}

import Prelude hiding (catch)
import Data.Either
import Text.Parsec hiding (ParseError(..))
--import Data.String.QQ
import qualified Messages as Mess
import Control.Monad.Error
import System.IO
import Control.Exception (IOException, bracket, catch)
import System.Directory (canonicalizePath)
import Graphics.UI.Gtk.Poppler.Document (documentGetNPages)

import Error
import Renderer (openPdfFile)

{-
test :: String
test = [s|
PdfFile blub.pdf
NumScreens 3
Start 1
SpeakerTracks 2
;
Node 1 1 2 3
Node 2 2 4 5
Node 3 3 4 5
;
2 u> 3 l> 1
1 <lr> 2 .. 5
1 <lr> 2 3 4 5
;
Screen native native
Screen
Screen
|]
-}

getViewerPresentationConfig knownClients fn = do
    when (not $ any (== "newnative") knownClients) $ fail "necessary client not found"
    doc <- openPdfFile fn
    npages <- liftIO $ documentGetNPages doc
    liftIO $ putStrLn $ "Found " ++ show npages ++ " pages..."
    let pageNumbers = [1 .. npages]
        edgesDown = zipWith (\x y -> (x, y, Mess.Down)) pageNumbers (tail pageNumbers)
        edgesUp = map (\(x, y, _) -> (y, x, Mess.Up)) edgesDown
    return (Header fn undefined 1 [], map (\x -> (x, [x])) pageNumbers, edgesDown ++ edgesUp, [[Screen "newnative" []]])

getPresentationConfig knownClients fn = do
    cont <- readFileError' fn
    (h, n, e, s) <- parseError (parseFile knownClients) ("PresentationConfig: " ++ fn) cont
    pFile <- liftIO $ canonicalizePath $ pdfFile h
    return (h { pdfFile = pFile }, n, e, s)

parseError parser sname str = either (throwError . ParseError) return res
    where res = parse parser sname str

horizWhite = oneOf " \t"
whitespace = many1 horizWhite
number = many1 digit
readNumber = number >>= return . read
nlOrEOF = (try newline >> return ()) <|> eof

parseFile knownClients = do
    h <- header
    spaces
    string ";"
    spaces
    n <- nodes $ paths h
    spaces
    string ";"
    spaces
    e <- edges
    spaces
    string ";"
    spaces
    s <- screens knownClients $ paths h
    spaces
    eof
    return (h, n, e, s)

data Screen = Screen String [String] deriving Show
screens knownClients numScreens = count numScreens (screen knownClients)
screen knownClients = do
    spaces
    string "Screen"
    option [] $ do
        whitespace
        clients <- choice knownClientsParsers `sepBy` whitespace
        nlOrEOF
        return clients
    where
    knownClientsParsers = flip map knownClients $ \clName -> try $ do
        clientName <- string clName
        args <- option [] $ do
            string "("
            args <- many1 alphaNum `sepBy` (spaces >> string "," >> spaces)
            string ")"
            return args
        return $ Screen clientName args

edges = many edge >>= return . concat
edge = do
    spaces
    try simple <|> complex
    where
    dir = oneOf "udlr"
    dirToNavCommand 'u' = Mess.Up
    dirToNavCommand 'd' = Mess.Down
    dirToNavCommand 'r' = Mess.Right
    dirToNavCommand 'l' = Mess.Left
    spaceSepList start =
            (try $ whitespace >> string ".." >> whitespace >> readNumber >>= \to -> nlOrEOF >> return [start .. to])
        <|> (try $ whitespace >> (readNumber `sepBy` whitespace) >>= \lst -> nlOrEOF >> return (start : lst))
        <|> (many horizWhite >> nlOrEOF >> return [start])
    arrowListToEdges arrow as = map (\(s, d) -> (s, d, dirToNavCommand arrow)) $ zip as (tail as)
    complex = do
        n1 <- readNumber
        whitespace
        string "<"
        arrow <- count 2 dir
        string ">"
        whitespace
        n2 <- readNumber
        ns <- spaceSepList n2
        let nodes = n1 : ns :: [Int]
        return $ arrowListToEdges (arrow !! 1) nodes ++ arrowListToEdges (arrow !! 0) (reverse nodes)
    simpleH old = (do
        whitespace
        arr <- dir >>= return . dirToNavCommand
        string ">"
        whitespace
        num <- readNumber
        res <- simpleH num
        return $ (old, num, arr) : res) <|> (nlOrEOF >> return [])
    simple = do
        n1 <- readNumber
        let _ = n1 :: Int
        simpleH n1

nodes numScreens = (node numScreens) `sepBy` spaces
node numScreens = do
    spaces
    string "Node"
    whitespace
    nodeId <- readNumber
    let _ = nodeId :: Int
    dsts <- count numScreens $ do
        whitespace
        n <- readNumber
        let _ = n :: Int
        return n
    nlOrEOF
    return (nodeId, dsts)

data Header = Header {
     pdfFile :: FilePath
    ,paths :: Int
    ,startNode ::  Int
    ,tracks :: [Int]
    } deriving Show
header = do
    [fname, numScreens, start, tracks] <- permutation [filename, numscreens, start, speakerTracks]
    return $ Header (head fname) (read $ head numScreens) (read $ head start) (map read tracks)
    where
    filename = try $ do
        spaces
        string "PdfFile"
        whitespace
        fname <- manyTill anyChar nlOrEOF
        return [fname]
    numscreens = try $ do
        spaces
        string "NumScreens"
        whitespace
        screens <- manyTill digit nlOrEOF
        return [screens]
    start = try $ do
        spaces
        string "Start"
        whitespace
        start <- manyTill digit nlOrEOF
        return [start]
    speakerTracks = try $ do
        spaces
        string "SpeakerTracks"
        tracks <- many $ do
            whitespace
            number
        nlOrEOF
        return tracks

permutation ps = helper 0 [] $ map Left ps
    where
    helper 0 tried [] = fail "no progress in permutation"
    helper _ tried [] | null lefts = return $ reverse rights
                      | otherwise = helper 0 [] (reverse tried)
        where
        (lefts, rights) = partitionEithers tried
    helper n tried (p : ps) = case p of
                                   Right _ -> helper n (p : tried) ps
                                   Left parser ->
                                       (try parser >>= \res -> helper (succ n) (Right res : tried) ps) <|> helper n (p : tried) ps
