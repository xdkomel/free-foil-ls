-- {-# LANGUAGE KindSignatures    #-}
-- {-# LANGUAGE TypeFamilies      #-}
-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs             #-}
-- {-# LANGUAGE LambdaCase        #-}
-- {-# LANGUAGE PatternSynonyms   #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE TypeOperators     #-}
-- {-# LANGUAGE DataKinds     #-}
-- {-# LANGUAGE ViewPatterns     #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE MultiParamTypeClasses                 #-}
-- {-# LANGUAGE PolyKinds                 #-}

-- module Common.OffsetScope where

-- import qualified Control.Monad.Foil as F
-- import qualified Control.Monad.Free.Foil as F
-- import Data.IntSet

-- newtype OffsetScope (n :: F.S) = UnsafeOffsetScope F.RawScope Int
--   deriving newtype NFData

-- -- shiftScope :: OffsetScope n -> OffsetScope l -> OffsetScope n
-- -- shiftScope (UnsafeOffsetScope n no) (UnsafeOffsetScope l lo)
-- --   let lMax = lo + (if IntSet.null l then -1 else IntSet.findMax l)
-- --       nScopedMax = if IntSet.null n then -1 else IntSet.findMax n
-- --       nMax = nScopedMax + no
-- --   in UnsafeOffsetScope n ((max lMax nMax) - nScopedMax)
-- shiftScope :: OffsetScope n -> Int -> OffsetScope n
-- shiftScope (UnsafeOffsetScope n no) o = UnsafeOffsetScope n (no + o)

-- extendScope :: F.NameBinder n l -> OffsetScope n -> OffsetScope l
-- extendScope (F.UnsafeNameBinder (F.UnsafeName name)) (UnsafeOffsetScope scope offset) =
--   UnsafeOffsetScope (IntSet.insert name scope) 0

-- rawFreshName :: F.RawScope -> Int -> F.RawName
-- rawFreshName scope offset = 
--   offset + 1 + (if IntSet.null scope then -1 else IntSet.findMax scope)

-- withFresh :: Distinct n
--   => OffsetScope n
--   -> (forall l. DExt n l => F.NameBinder n l -> r) 
--   -> r
-- withFresh (UnsafeOffsetScope scope offset) cont = 
--   cont (UnsafeNameBinder (UnsafeName (rawFreshName scope offset)))

-- withRefreshedPattern' :: (F.CoSinkable pattern, F.Distinct o, F.InjectName e, F.Sinkable e)
--   => Scope o
--   -> pattern n l
--   -> (forall o'. DExt o o' => ((F.Name n -> e o) -> F.Name l -> e o') -> pattern o o' -> r) -> r
-- withRefreshedPattern' scope pattern cont = withPattern
--   (\scope' binder f -> withRefreshed scope' (nameOf binder)
--     (\binder' ->
--       let k subst name = case unsinkName binder name of
--               Nothing    -> injectName (nameOf binder')
--               Just name' -> sink (subst name')
--        in f (WithRefreshedPattern' k) binder'))
--   idWithRefreshedPattern'
--   compWithRefreshedPattern'
--   scope
--   pattern
--   (\(WithRefreshedPattern' f) pattern' -> cont f pattern')
