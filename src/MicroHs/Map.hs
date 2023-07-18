-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Map(module MicroHs.Map) where
import Prelude
--Ximport Compat

{-
import qualified Data.Map as M
import GHC.Classes(Ord)
import qualified GHC.Maybe

type Map k v = M.Map k v

insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert = M.insert

fromListWith :: (Ord k) => (k -> k -> Bool) -> (v -> v -> v) -> [(k, v)] -> Map k v
fromListWith _ = M.fromListWith

fromList :: (Ord k) => [(k, v)] -> Map k v
fromList = M.fromList

union :: (Ord k) => Map k v -> Map k v -> Map k v
union = M.union

lookup :: (Ord k) => (k -> k -> Bool) -> k -> Map k v -> Maybe v
lookup _ k m =
  case M.lookup k m of
    GHC.Maybe.Nothing -> Nothing
    GHC.Maybe.Just v -> Just v

empty :: Map k v
empty = M.empty

elems :: Map k v -> [v]
elems = M.elems
-}

-- This is a pretty bad implementation.
data Map k v = Map [(k, v)]
  --Xderiving(Show)

insert :: k -> v -> Map k v -> Map k v
insert k v amap =
  case amap of
    Map kvs -> Map ((k, v):kvs)

fromListWith :: (k -> k -> Bool) -> (v -> v -> v) -> [(k, v)] -> Map k v
fromListWith eq un =
  let
    ins ikv akvs =
      case akvs of
        [] -> [ikv]
        kv : kvs ->
          if eq (fst ikv) (fst kv) then
            (fst ikv, un (snd ikv) (snd kv)) : kvs
          else
            kv : ins ikv kvs
  in
     Map . foldr ins []

fromList :: [(k, v)] -> Map k v
fromList = Map

union :: Map k v -> Map k v -> Map k v
union akvs1 akvs2 =
  case akvs1 of
    Map kvs1 ->
      case akvs2 of
        Map kvs2 -> Map (kvs1 ++ kvs2)

lookup :: (k -> k -> Bool) -> k -> Map k v -> Maybe v
lookup eq k akvs =
  case akvs of
    Map kvs -> lookupBy eq k kvs

empty :: Map k v
empty = Map []

elems :: Map k v -> [v]
elems m =
  case m of
    Map kvs -> map snd kvs