{-# LANGUAGE RecordWildCards #-}
module Logic where

import BSP
import Collision
import Game
import Items
import Entity
import Data.Bits
import Data.Vect hiding (Vector)
import Data.Maybe

import Data.Vector

data GameState
  = GameState
  { entities  :: Vector Entity
  }
  deriving Show

_CONTENTS_SOLID = 1 :: Int -- an eye is never valid in a solid
_MASK_SOLID     = _CONTENTS_SOLID

createEntities bsp ents = fromList $ addItem [] ents
  where
    addItem l [] = l
    addItem l (x:xs) = case findIndex (\i -> classname x == itClassName i) items of
      Nothing -> addItem l xs
      Just i  -> addItem l' xs where
        EntityData{..} = x
        item@Item{..} = items ! i
        e p = EItem
            { random      = fromMaybe 0 random
            , wait        = fromMaybe 0 wait
            , spawnflags  = spawnflags
            , origin      = p
            , count       = itQuantity
            , item        = item
            }
        itemRadius = 15
        maxs = Vec3 itemRadius itemRadius itemRadius
        mins = neg maxs
        end  = origin - Vec3 0 0 (-4096)
        itemOrigin = case spawnflags .&. 1 == 1 of
          True  -> Just origin -- suspended item
          False -> do -- drop to floor
            (endpos,TraceHit{..}) <- traceBox mins maxs bsp origin end _MASK_SOLID
            if outputStartsOut then return endpos else Nothing
        l' = case itemOrigin of
          Nothing -> l
          Just p  -> e p:l

loadLevel = do
  let mapName = "q3dm6.bsp"
  bsp@BSPLevel{..} <- loadBSP mapName
  ents <- case parseEntities mapName blEntities of
    Left err -> fail err
    Right xs -> return $ createEntities bsp xs

  return $ GameState ents
