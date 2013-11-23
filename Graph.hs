module Graph where

import qualified Data.Graph.Inductive.Graph as FGL
import Data.Graph.Inductive.Tree

{- Structure of our graph
- After much hassle with the documentation of the functionally great FGL package
- (mostly what exactly match should do), we decided to use a finished structure
- instead of a usage (in terms of complexity) optimized one that was buggy because
- of match (too imprecise documentation)
-}
constructGraph :: [(Int, nlabel)] -> [(Int, Int, elabel)] -> Gr nlabel elabel
constructGraph nodes edges = foldr FGL.insEdge (foldr FGL.insNode FGL.empty nodes) edges
