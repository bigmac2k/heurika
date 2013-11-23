{-# LANGUAGE ViewPatterns #-}
module PdfGraph (constructGraph, bfs) where

{- Mainly contains algorithm to generate worklist
- lazy breath first search that optionally gets a scope
- explore all nodes (in edge labeled order) in breadth first fashion up to scope
-}

import Prelude hiding (
    {- Traversable -} mapM, sequence,
    {- Foldable -} foldl, foldr, foldl1, foldr1, concat, concatMap, and, or, any, all, sum, product, maximum, minimum, elem, notElem
    )
import Data.Traversable
import Data.Foldable

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>), (><), viewl, viewr, ViewL(..), ViewR(..))
import qualified Data.Graph.Inductive.Graph as FGL

import Graph
import qualified Messages as Mess

{-
import Debug.Trace (trace)
import Data.GraphViz (preview)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as TL

instance Labellable a => Labellable [a] where
    toLabelValue = StrLabel . foldl (\acc l -> acc `TL.append` (TL.pack . show $ toLabelValue l)) TL.empty
instance Labellable Mess.NavCommand where
    toLabelValue = StrLabel . TL.pack . show
-}

bfs :: (Enum t2, Eq t2, Num t2, Ord t1, FGL.Graph gr) => gr t t1 -> Maybe t2 -> FGL.Node -> [FGL.Node]
bfs graph depth node = helper (Set.singleton node) (succ `fmap` depth) (Seq.singleton node)
    where
    toEdges n = let (_, _, _, adjTo) = FGL.context graph n in Seq.fromList adjTo
    sortEdges = Seq.sortBy $ \(lbl1, _) (lbl2, _) -> compare lbl1 lbl2
    nodesFromEdges = fmap snd
    sortedNodes = nodesFromEdges . sortEdges . toEdges
    helper _ (Just 0) _ = []
    helper _ _ (viewl -> EmptyL) = []
    helper markedSet depth (viewl -> n :< wlBuf) =
        let adjNodes = Seq.filter (not . flip Set.member markedSet) $ sortedNodes n in
            n : helper (foldr Set.insert markedSet adjNodes) (pred `fmap` depth) (wlBuf >< adjNodes)

test1 n = constructGraph (zip nodes nodes) (edges ++ map invert edges)
    where invert (x, y, Mess.Up) = (y, x, Mess.Down)
          invert (x, y, Mess.Down) = (y, x, Mess.Up)
          nodes = [0..n]
          edges = zipWith (\x y -> (x, y, Mess.Down)) nodes $ tail nodes
test2 = constructGraph (map (\x -> (x, x)) [1..9]) edges
    where edges = [(1,8,Mess.Right)
                  ,(8,1,Mess.Left)
                  ,(8,9,Mess.Right)
                  ,(9,8,Mess.Left)
                  ,(8,4,Mess.Down)
                  ,(4,8,Mess.Up)
                  ,(9,4,Mess.Down)
                  ,(4,9,Mess.Right)
                  ,(2,4,Mess.Right)
                  ,(4,2,Mess.Left)
                  ,(2,3,Mess.Down)
                  ,(3,2,Mess.Up)
                  ,(3,5,Mess.Right)
                  ,(5,3,Mess.Left)
                  ,(3,7,Mess.Down)
                  ,(7,3,Mess.Up)
                  ,(5,6,Mess.Down)
                  ,(6,5,Mess.Up)]
