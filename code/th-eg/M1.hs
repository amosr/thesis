{-# LANGUAGE TemplateHaskell #-}
module M1 where
import qualified Language.Haskell.TH as TH


power :: Int -> TH.Q (TH.TExp (Int -> Int))
power 0 = [||\i -> 1                       ||]
power n = [||\i -> $$(power (n - 1)) i * i ||]


powerS' :: Int -> TH.Q (TH.TExp Int) -> TH.Q (TH.TExp Int)
powerS' 0 i = [||1||]
powerS' n i = [|| $$(powerS' (n-1) i) * $$(i)||]

powerS :: Int -> TH.Q (TH.TExp (Int -> Int))
powerS n = [|| \i -> $$(powerS' n [||i||]) ||]


powerE' :: Int -> TH.Q (TH.TExp Int) -> TH.Q (TH.TExp Int)
powerE' 0 i = [||1||]
powerE' n i
 | n `mod` 2 == 0
 = [|| let a = $$(powerE' (n `div` 2) i)
       in  a * a ||]
 | otherwise
 = [|| $$(powerE' (n-1) i) * $$(i)||]

powerE :: Int -> TH.Q (TH.TExp (Int -> Int))
powerE n = [|| \i -> $$(powerE' n [||i||]) ||]



run :: IO ()
run = do
 a <- TH.runQ (powerS 5)
 print $ TH.ppr $ TH.unType a

