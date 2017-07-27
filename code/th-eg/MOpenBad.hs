{-# LANGUAGE TemplateHaskell #-}
module MOpenBad where
import qualified Language.Haskell.TH as TH
import Data.IORef


powerBad :: Int -> TH.Q (TH.TExp Int)
powerBad n = do
  i0  <- [||0||]
  ref <- TH.runIO $ newIORef i0
  [|| \i -> $$(sneaky ref [||i||]) ||]
  TH.runIO $ readIORef ref
 where
  sneaky ref iq = do
    ix <- iq
    TH.runIO $ writeIORef ref ix
    return ix



run :: IO ()
run = do
 a <- TH.runQ (powerBad 5)
 print $ TH.ppr $ TH.unType a

