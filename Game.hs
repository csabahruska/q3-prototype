module Game where

import Data.Vect
import Data.Vector

import Items

{-
  no networking
  client server architecture
    client - sends events to server
    server - sends snapshot to client which draws it to the screen

TODO:
  create event type (closed)
  create entity type (closed)
  create player state (record)
  create client server snapshot loop skeleton
    client --> server : client command/event
    client <-- server : snapshot

  create minimal player and entity state
    player: position, angles, velocity

GOAL:
  minimal playable game
  jump pads
  triggers (kill triggers)
  buttons
  doors
  items: health, armor, ammos
  weapons
  bullets
  powerups: quad damage, etc

-}

data EntityType
  = ET_GENERAL
  | ET_PLAYER
  | ET_ITEM
  | ET_MISSILE
  | ET_MOVER
  | ET_BEAM
  | ET_PORTAL
  | ET_SPEAKER
  | ET_PUSH_TRIGGER
  | ET_TELEPORT_TRIGGER
  | ET_INVISIBLE
  | ET_GRAPPLE  -- grapple hooked on wall
  | ET_TEAM
{-
  | ET_EVENTS				// any of the EV_* events can be added freestanding
							// by setting eType to ET_EVENTS + eventNum
							// this avoids having to set eFlags and eventNum
-}

data EntityEvent
  = EV_NONE

  | EV_FOOTSTEP
  | EV_FOOTSTEP_METAL
  | EV_FOOTSPLASH
  | EV_FOOTWADE
  | EV_SWIM

  | EV_STEP_4
  | EV_STEP_8
  | EV_STEP_12
  | EV_STEP_16

  | EV_FALL_SHORT
  | EV_FALL_MEDIUM
  | EV_FALL_FAR

  | EV_JUMP_PAD -- boing sound at origin, jump sound on player

  | EV_JUMP
  | EV_WATER_TOUCH  -- foot touches
  | EV_WATER_LEAVE  -- foot leaves
  | EV_WATER_UNDER  -- head touches
  | EV_WATER_CLEAR  -- head leaves

  | EV_ITEM_PICKUP  -- normal item pickups are predictable
  | EV_GLOBAL_ITEM_PICKUP -- powerup / team sounds are broadcast to everyone

  | EV_NOAMMO
  | EV_CHANGE_WEAPON
  | EV_FIRE_WEAPON

  | EV_USE_ITEM0
  | EV_USE_ITEM1
  | EV_USE_ITEM2
  | EV_USE_ITEM3
  | EV_USE_ITEM4
  | EV_USE_ITEM5
  | EV_USE_ITEM6
  | EV_USE_ITEM7
  | EV_USE_ITEM8
  | EV_USE_ITEM9
  | EV_USE_ITEM10
  | EV_USE_ITEM11
  | EV_USE_ITEM12
  | EV_USE_ITEM13
  | EV_USE_ITEM14
  | EV_USE_ITEM15

  | EV_ITEM_RESPAWN
  | EV_ITEM_POP
  | EV_PLAYER_TELEPORT_IN
  | EV_PLAYER_TELEPORT_OUT

  | EV_GRENADE_BOUNCE -- eventParm will be the soundindex

  | EV_GENERAL_SOUND
  | EV_GLOBAL_SOUND -- no attenuation
  | EV_GLOBAL_TEAM_SOUND

  | EV_BULLET_HIT_FLESH
  | EV_BULLET_HIT_WALL

  | EV_MISSILE_HIT
  | EV_MISSILE_MISS
  | EV_MISSILE_MISS_METAL
  | EV_RAILTRAIL
  | EV_SHOTGUN
  | EV_BULLET -- otherEntity is the shooter

  | EV_PAIN
  | EV_DEATH1
  | EV_DEATH2
  | EV_DEATH3
  | EV_OBITUARY

  | EV_POWERUP_QUAD
  | EV_POWERUP_BATTLESUIT
  | EV_POWERUP_REGEN

  | EV_GIB_PLAYER -- gib a previously living player
  | EV_SCOREPLUM -- score plum
{-
//#ifdef MISSIONPACK
	EV_PROXIMITY_MINE_STICK,
	EV_PROXIMITY_MINE_TRIGGER,
	EV_KAMIKAZE,			// kamikaze explodes
	EV_OBELISKEXPLODE,		// obelisk explodes
	EV_OBELISKPAIN,			// obelisk is in pain
	EV_INVUL_IMPACT,		// invulnerability sphere impact
	EV_JUICED,				// invulnerability juiced effect
	EV_LIGHTNINGBOLT,		// lightning bolt bounced of invulnerability sphere
//#endif
-}
  | EV_DEBUG_LINE
  | EV_STOPLOOPINGSOUND
  | EV_TAUNT
  | EV_TAUNT_YES
  | EV_TAUNT_NO
  | EV_TAUNT_FOLLOWME
  | EV_TAUNT_GETFLAG
  | EV_TAUNT_GUARDBASE
  | EV_TAUNT_PATROL

data PlayerState
  = PlayerState
  { -- stats: health, holdable item, weapons (bitmask), armor, dead yaw, clients ready to exit intermission (bitmask), max health (health/armor limit)
    -- persistant (not cleared on respawn): score, hits, rank, team, spawn count, attacker, attackee armor, killed, impressive count, excellent count, defend count,
    --                                      assists count, grauntlet frag count, captures
  }

data EntityFlags
  = EF_DEAD -- don't draw a foe marker over players with EF_DEAD
  | EF_TELEPORT_BIT     -- toggled every time the origin abruptly changes
  | EF_AWARD_EXCELLENT  -- draw an excellent sprite
  | EF_PLAYER_EVENT
  | EF_BOUNCE           -- for missiles
  | EF_BOUNCE_HALF      -- for missiles
  | EF_AWARD_GAUNTLET   -- draw a gauntlet sprite
  | EF_NODRAW           -- may have an event, but no model (unspawned items)
  | EF_FIRING           -- for lightning gun
  | EF_KAMIKAZE
  | EF_MOVER_STOP       -- will push otherwise
  | EF_AWARD_CAP        -- draw the capture sprite
  | EF_TALK             -- draw a talk balloon
  | EF_CONNECTION       -- draw a connection trouble sprite
  | EF_VOTED            -- already cast a vote
  | EF_AWARD_IMPRESSIVE -- draw an impressive sprite
  | EF_AWARD_DEFEND     -- draw a defend sprite
  | EF_AWARD_ASSIST     -- draw a assist sprite
  | EF_AWARD_DENIED     -- denied
  | EF_TEAMVOTED        -- already cast a team vote

data EntityState
  = EntityState
  { number  :: Int -- entity index
  , eType   :: EntityType
  , eFlags  :: [EntityFlags]
--  , pos     :: Trajectory -- position
--  , apos    :: Trajectory -- angles
  , event   :: EntityEvent
  }

data MeansOfDeath
  = MOD_UNKNOWN
  | MOD_SHOTGUN
  | MOD_GAUNTLET
  | MOD_MACHINEGUN
  | MOD_GRENADE
  | MOD_GRENADE_SPLASH
  | MOD_ROCKET
  | MOD_ROCKET_SPLASH
  | MOD_PLASMA
  | MOD_PLASMA_SPLASH
  | MOD_RAILGUN
  | MOD_LIGHTNING
  | MOD_BFG
  | MOD_BFG_SPLASH
  | MOD_WATER
  | MOD_SLIME
  | MOD_LAVA
  | MOD_CRUSH
  | MOD_TELEFRAG
  | MOD_FALLING
  | MOD_SUICIDE
  | MOD_TARGET_LASER
  | MOD_TRIGGER_HURT
  | MOD_GRAPPLE

data GameType
  = GT_FFA            -- free for all
  | GT_TOURNAMENT     -- one on one tournament
  | GT_SINGLE_PLAYER  -- single player ffa

  -- team games go after this --

  | GT_TEAM       -- team deathmatch
  | GT_CTF        -- capture the flag
  | GT_1FCTF
  | GT_OBELISK
  | GT_HARVESTER
  | GT_MAX_GAME_TYPE

data Snapshot
  = Snapshot
  { playerState :: PlayerState
  , entities    :: [EntityState]
  }

data UserCommand
  = UserCommand
  { serverTime  :: Int
  , angles      :: Vec3
  , buttons     :: Int
  , weapon      :: Int
  , forwardMove :: Char
  , rightMove   :: Char
  , upMove      :: Char
  }

stepWorld :: UserCommand -> Snapshot -> Snapshot
stepWorld = undefined

pmove :: UserCommand -> PlayerState -> PlayerState
pmove = undefined

{-
  entity actions
    think   :: Entity -> _    -- used for action sequencing
    reached :: Entity -> _    -- movers call this when hitting endpoint (only movers)
    blocked :: Entity (self) -> Entity (other) -> _   -- movers and doors
    touch   :: Entity (self) -> Entity (other) -> Trace -> _
    use     :: Entity (self) -> Entity (other) -> Entity (activator) -> _
    pain    :: Entity (self) -> Entity (attacker) -> Int (damage) -> _
    die     :: Entity (self) -> Entity (inflictor) -> Entity (attacker) -> Int (damage) -> MeansOfDeath -> _
-}
data Entity
  = EItem
    { random      :: Float
    , wait        :: Float
    , spawnflags  :: Int
    , origin      :: Vec3       -- varying
    , count       :: Int        -- varying (IT_WEAPON, IT_AMMO, IT_HEALTH, IT_POWERUP)
    , item        :: Item
    -- , itemType    :: ItemType
    -- , itemState   :: ItemState  -- varying
    }
  | EPlayer
    { 
    }
    deriving Show
{-
  TODO:
    distinguish:
      static info
      time varying value (state)
-}
{-
data ItemType
  = IT_WEAPON
  | IT_AMMO
  | IT_ARMOR
  | IT_HEALTH
  | IT_POWERUP
  | IT_TEAM
  | IT_HOLDABLE

data ItemState
  = IS_WEAPON   Int -- count (in case of dropped item)
  | IS_AMMO     Int -- count (in case of dropped item)
  | IS_ARMOR
  | IS_HEALTH   Int -- count (in case of dropped item)
  | IS_POWERUP  Int -- count (in case of dropped item)
  | IS_TEAM
  | IS_HOLDABLE
-}
-- question: where to put semantics?

{-
  use cases for item entity
    checked - spawn process
    simulation step
    touch process
-}