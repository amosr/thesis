{-# LANGUAGE TemplateHaskell #-}
module M2 where
import M1
import qualified Language.Haskell.TH as TH

power2 :: Int -> Int
power2 = $$(power 2)

powerS2 :: Int -> Int
powerS2 = $$(powerS 2)

powerE2 :: Int -> Int
powerE2 = $$(powerE 2)


