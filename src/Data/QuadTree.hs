module Data.QuadTree
  ( -- * Important types
    QuadTree
  , Region (..)
  , V2 (..)

    -- * Construction
  , makeTree
  , insert
  , fill

    -- * Destruction
  , foldTree
  , tightlySatisfying
  , hitTest
  , pointMap
  , getLocation
  , asWeighted
  , tile

  -- * Modifying
  , renormalize
  , tighten
  , cookieCut

    -- * Debugging
  , showTree

    -- * Helpers
  , bounds
  , inBounds

    -- * Subdivision
  , Quad (..)
  , subdivide
  , corners

    -- * Geometry
  , containsRegion
  , containsPoint
  , intersects
  , getIntersection
  , regionSize
  , regionPoints
  ) where

import           Control.Arrow (first)
import           Data.Bool (bool)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid
import           Data.QuadTree.Geometry
import           Data.QuadTree.Internal (Quadrant, Squadrant)
import qualified Data.QuadTree.Internal as I
import           GHC.Generics (Generic)
import           Linear.V2


data QuadTree a = QuadTree
  { qt_quad :: Quadrant a
  , qt_size :: Region
  }
  deriving (Show, Read, Eq, Functor, Generic, Traversable)
  -- deriving (Semigroup, Monoid) via Ap QuadTree a

instance (Monoid a) => Semigroup (QuadTree a) where
  QuadTree q1 r1 <> QuadTree q2 r2
    | r1 == r2
    = QuadTree (q1 <> q2) r1
    | otherwise
    = flip (foldr (uncurry fill)) (I.tile (r2, q2))
    $ flip (foldr (uncurry fill)) (I.tile (r2, q1))
    $ makeTree
    $ r1 <> r2


-- instance Applicative QuadTree where
--   pure a = QuadTree (pure a) mempty
--   QuadTree fq fr <*> QuadTree aq ar = QuadTree (fq <*> aq) (fr <> ar)

instance Foldable QuadTree where
  foldMap = foldTree . const


foldTree :: Monoid m => (Region -> a -> m) -> QuadTree a -> m
foldTree f = I.foldTree f . regionify


fill :: Monoid a => Region -> a -> QuadTree a -> QuadTree a
fill r a = liftTree $ I.fill a r


insert :: Monoid a => a -> V2 Int -> QuadTree a -> QuadTree a
insert r a = liftTree $ I.insert r a


getLocation :: Monoid a => QuadTree a -> V2 Int -> a
getLocation q v =  I.getLocation v $ regionify q


regionify :: QuadTree a -> Squadrant a
regionify (QuadTree q r) = (r, q)


showTree :: (a -> Char) -> QuadTree a -> String
showTree f q@(QuadTree _ (I.Region x y w h)) = do
  let pm = pointMap q
  unlines $ do
    yp <- [y .. y + h - 1]
    pure $ do
      xp <- [x .. x + w - 1]
      let p = V2 xp yp
      case M.lookup p pm of
        Nothing -> error $ "indexed a bad spot! " <> show p
        Just a -> pure $ f a


asWeighted :: Show a => QuadTree a -> [a]
asWeighted = (uncurry replicate . first I.regionSize  =<<) . tile


makeTree :: Monoid a => Region -> QuadTree a
makeTree r = QuadTree (I.Leaf mempty) r


liftTree :: (Squadrant a -> Quadrant a) -> QuadTree a -> QuadTree a
liftTree f w = w { qt_quad = f $ regionify w }


tile :: QuadTree a -> [(Region, a)]
tile = I.tile . regionify


pointMap :: QuadTree a -> Map (V2 Int) a
pointMap
  = foldTree (\r a -> M.fromList $ fmap (, a) $ regionPoints r)


hitTest :: Monoid m => (a -> m) -> Region -> QuadTree a -> m
hitTest f r = I.hitTest (const f) r . regionify


bounds :: QuadTree a -> Region
bounds = qt_size


inBounds :: QuadTree a -> Region -> Bool
inBounds = containsRegion . bounds


tightlySatisfying :: (a -> Bool) -> QuadTree a -> Region
tightlySatisfying f =
  foldTree $ \r a -> bool mempty r $ f a


------------------------------------------------------------------------------
-- | Map the space contained by the QuadTree.
--
-- $O(1)$
renormalize :: (Region -> Region) -> QuadTree a -> QuadTree a
renormalize f (QuadTree q r) = QuadTree q $ f r


------------------------------------------------------------------------------
-- | Change the bounds of the QuadTree to be a bounding box satisfying the
-- predicate.
--
-- $O(n)$
tighten :: Monoid a => (a -> Bool) -> QuadTree a -> QuadTree a
tighten f q = cookieCut (tightlySatisfying f q) q


------------------------------------------------------------------------------
-- | Cut out the given region of the QuadTree.
--
-- $O(n)$
cookieCut :: Monoid a => Region -> QuadTree a -> QuadTree a
cookieCut r q = do
  let pm = tile q
      keep = mapMaybe (\(r', a) -> fmap (, a) $ getIntersection r r') pm
      q' = makeTree r
  foldr (uncurry fill) q' keep


-- test1 :: QuadTree String
-- test1
--   = flip QuadTree (Region 0 0 16 16)
--   $ I.Node
--   $ Quad (I.Leaf "tl1") (I.Leaf "tr1") (I.Leaf "bl1") (I.Leaf "br1")

-- test2 :: QuadTree String
-- test2
--   = flip QuadTree (Region 0 0 32 32)
--   $ I.Node
--   $ Quad (I.Leaf "tl2") (I.Leaf "tr2") (I.Leaf "bl2") (I.Leaf "br2")

