module S where

import qualified Data.Map as Map
import Control.Monad.State

data S k v
 = S
 { sPairs :: Map.Map (v, v) k
 , sBinds :: Map.Map k v
 , sFresh :: Int -> k
 }

fixbind :: (Ord k, Ord v) => (v,v) -> State (S k v) v -> State (S k v) k
fixbind ks compute = do
  s <- get
  let m = sPairs s
  case Map.lookup ks m of
   Nothing -> do
    let x  = sFresh s $ Map.size m
    let m' = Map.insert ks x m
    put s { sPairs = m' }
    b <- compute
    bind x b
    return x
   Just v ->
    return v

bind :: Ord k => k -> v -> State (S k v) ()
bind x p = do
 s <- get
 put s { sBinds = Map.insert x p $ sBinds s }

runS :: Ord k => (Int -> k) -> State (S k v) r -> (Map.Map k v, r)
runS fresh m
 = let (r,s) = runState m $ S Map.empty Map.empty fresh
   in  (sBinds s, r)

