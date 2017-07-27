{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}

module N3 where

import S

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace

-- "Expressions"
data E x = EVar x -- TODO | EVal V
 deriving (Eq, Ord, Show)

-- Process head
data H x c
 = Read x c
 | Send c (E x)
 | Set  x (E x)
 deriving (Eq, Ord, Show)

-- Process body
data P x c
 = H x c :> P x c
 | P x c :+ P x c
 | Jump x
 | Fail
 | Done
 | If (E x) (P x c) (P x c)
 deriving (Eq, Ord, Show)
infixr :>

-- Construct a non-deterministic choice with some local simplification.
choice :: (Eq x, Eq c) => P x c -> P x c -> P x c
choice p q
   -- (0 + q) = q
   | Fail <- p
   = q
   -- (p + 0) = p
   | Fail <- q
   = p
   -- Pull out common parts.
   -- p + p = p
   | p == q
   = p
   --  (read c; p) + (read c; q)
   -- is equivalent to
   --  read c; (p + q)
   | hp :> p' <- p
   , hq :> q' <- q
   , hp == hq
   = hp :> choice p' q'
   | otherwise
   = p :+ q

-- Process with jump definitions
data Tails x c
 = Tails (Map.Map x (P x c)) (P x c)
 deriving (Show)

data Top x c = Top
  { topIns     :: Set.Set c
  , topOuts    :: Set.Set c
  , topTails   :: Tails x c
  }
 deriving (Show)

topChans :: Ord c => Top x c -> Set.Set c
topChans t = Set.union (topIns t) (topOuts t)

productTop :: Top String String -> Top String String -> Top String String
productTop t1 t2
 = let shared = Set.intersection (topChans t1) (topChans t2)
       outs   = Set.union (topOuts t1) (topOuts t2)
       ins    = Set.union (topIns  t1) (topIns  t2) `Set.difference` outs
       tails  = productTails show shared (topTails t1) (topTails t2)
   in Top ins outs tails


productTails :: (Ord x, Ord c) => (Int -> x) -> Set.Set c -> Tails x c -> Tails x c -> Tails x c
productTails fresh cs tp tq
 = let (t,p) = runS fresh (productTails' cs tp tq)
   in Tails t p

productTails' :: (Ord x, Ord c) => Set.Set c -> Tails x c -> Tails x c -> State (S x (P x c)) (P x c)
productTails' cs (Tails tp p0) (Tails tq q0) = comm p0 q0
 where
  comm p q
   -- Unfold jumps before starting
   | Jump {} <- p
   = step True  (p,q)
   | Jump {} <- q
   = step False (q,p)
   | Done    <- p
   = step False (q,p)
   | Done    <- q
   = step True  (p,q)
   | otherwise = do
     p' <- step True  (p,q)
     q' <- step False (q,p)
     return $ choice p' q'

  step this (p,q) = do
   let go p' q' = if   this
                  then comm p' q'
                  else comm q' p'
   case p of
    Read x c :> p'
     -- Read/write
     | Set.member c cs
     , Send c' e :> q' <- q
     , c == c'
     -> do pq <- go p' q'
           return (Set x e :> Send c (EVar x) :> pq)
     -- Read/read
     | Set.member c cs
     , Read x' c' :> q' <- q
     , c == c'
     -> do pq <- go p' q'
           return (Read x c :> Set x' (EVar x') :> pq)
     | Set.member c cs
     -> return Fail
     | otherwise
     -> (Read x c :>) <$> go p' q

    Send c e :> p'
     -- Fail here because it's handled by above commutative case
     | Set.member c cs
     -> return Fail
     | otherwise
     -> (Send c e :>) <$> go p' q

    Set x e :> p'
     -> (Set x e :>) <$> go p' q

    a :+ b
     -> (:+) <$> go a q <*> go b q

    Fail
     -> return Fail
    If e tt ff
     -> (If e) <$> go tt q <*> go ff q

    Done
     -> return q

    Jump x -> do
      let tails   = if this then tp else tq
      let Just p' = Map.lookup x tails
      let k       = if this then (p,q) else (q,p)
      x' <- fixbind k (go p' q)
      return (Jump x')


copy1 :: String -> String -> Top String String
copy1 ci co = Top
 { topIns = Set.singleton ci
 , topOuts = Set.singleton co
 , topTails = Tails 
    (Map.fromList [(go, Read buf ci :> Send co (EVar buf) :> Jump go)])
    (Jump go)
 }
 where
  go = ci ++ "$go"
  buf = ci ++ "$x"

copy2' = productTop (copy1 "a" "b") (copy1 "b" "c")

read2 :: String -> String -> Top String String
read2 ci1 ci2 = Top
 { topIns = Set.fromList [ci1, ci2]
 , topOuts = Set.empty
 , topTails = Tails 
    (Map.fromList [(go, Read buf1 ci1 :> Read buf2 ci2 :> Jump go)])
    (Jump go)
 }
 where
  go = ci1 ++ "$" ++ ci2 ++ "$go"
  buf1= ci1 ++ "$x"
  buf2= ci2 ++ "$x"

read2' = productTop (read2 "a" "b") (read2 "b" "c")

