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

spawnItem bsp EntityData{..} i = do
  itemOrigin <- case spawnflags .&. 1 == 1 of
    True  -> Just origin  -- suspended item
    False -> do           -- drop to floor
      let itemRadius = 15
          maxs = Vec3 itemRadius itemRadius itemRadius
          mins = neg maxs
          end  = origin - Vec3 0 0 (-4096)
      (endpos,TraceHit{..}) <- traceBox mins maxs bsp origin end _MASK_SOLID
      if outputStartsOut then return endpos else Nothing
  let item@Item{..} = items ! i
  return $ EItem
    { random      = fromMaybe 0 random
    , wait        = fromMaybe 0 wait
    , spawnflags  = spawnflags
    , origin      = itemOrigin
    , count       = itQuantity
    , item        = item
    }

spawnEntities bsp ents = fromList . catMaybes $ fmap spawnEntity ents
  where
    spawnEntity x@EntityData{..} = case findIndex (\i -> classname == itClassName i) items of
      Just i  -> spawnItem bsp x i
      Nothing -> case classname of
        -- TODO: add spawn entities
        _ -> Nothing

loadLevel = do
  let mapName = "q3dm6.bsp"
  bsp@BSPLevel{..} <- loadBSP mapName
  ents <- case parseEntities mapName blEntities of
    Left err -> fail err
    Right xs -> return $ spawnEntities bsp xs

  return $ GameState ents
