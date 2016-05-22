{-# LANGUAGE LambdaCase #-}
module Entity where

import Control.Monad (void)
import Text.Megaparsec hiding (count)
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vect

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol = L.symbol sc

quoted = between (lexeme $ char '"') (lexeme $ char '"')

stringLiteral = lexeme $ char '"' >> manyTill anyChar (char '"')
integerLiteral = fromIntegral <$> L.signed sc (lexeme L.integer)
floatLiteral = realToFrac <$> L.signed sc (lexeme $ try L.float <|> fromIntegral <$> L.integer)

vector3 = Vec3 <$> floatLiteral <*> floatLiteral <*> floatLiteral

-- quake 3 entity parser
parseEntities fname src = case parse entities fname src of
  Left err  -> Left (parseErrorPretty err)
  Right e   -> Right e

entities :: Parser [EntityData]
entities = sc *> many entity <* eof

entity :: Parser EntityData
entity = foldr ($) defaultEntityData <$> between (symbol "{") (symbol "}") (some value)

value :: Parser (EntityData -> EntityData)
value = stringLiteral >>= \case
  "classname"   -> (\v e -> e {classname = v}) <$> stringLiteral
  "model"       -> (\v e -> e {model = Just v}) <$> stringLiteral
  "model2"      -> (\v e -> e {model2 = Just v}) <$> stringLiteral
  "target"      -> (\v e -> e {target = Just v}) <$> stringLiteral
  "targetname"  -> (\v e -> e {targetname = Just v}) <$> stringLiteral
  "team"        -> (\v e -> e {team = Just v}) <$> stringLiteral
  "targetShaderName"    -> (\v e -> e {targetShaderName = Just v}) <$> stringLiteral
  "targetShaderNewName" -> (\v e -> e {targetShaderNewName = Just v}) <$> stringLiteral

  "spawnflags"  -> (\v e -> e {spawnflags = v}) <$> quoted integerLiteral

  "origin"      -> (\v e -> e {origin = v}) <$> quoted vector3
  "angles"      -> (\v e -> e {angles = v}) <$> quoted vector3

  "angle"       -> (\v e -> e {angles = Vec3 0 v 0}) <$> quoted floatLiteral

  "notsingle"   -> (\v e -> e {notsingle = v /= 0}) <$> quoted integerLiteral
  "notteam"     -> (\v e -> e {notteam = v /= 0}) <$> quoted integerLiteral
  "notfree"     -> (\v e -> e {notfree = v /= 0}) <$> quoted integerLiteral
  "notq3a"      -> (\v e -> e {notq3a = v /= 0}) <$> quoted integerLiteral
  "gametype"    -> (\v e -> e {gametype = Just v}) <$> stringLiteral

-- custom; varying defaults
  "message"     -> (\v e -> e {message = Just v}) <$> stringLiteral
  "noise"       -> (\v e -> e {noise = Just v}) <$> stringLiteral
  "music"       -> (\v e -> e {music = Just v}) <$> stringLiteral

  "speed"       -> (\v e -> e {speed = Just v}) <$> quoted floatLiteral
  "wait"        -> (\v e -> e {wait = Just v}) <$> quoted floatLiteral
  "random"      -> (\v e -> e {random = Just v}) <$> quoted floatLiteral
  "gravity"     -> (\v e -> e {gravity = Just v}) <$> quoted floatLiteral
  "roll"        -> (\v e -> e {roll = Just v}) <$> quoted floatLiteral
  "light"       -> (\v e -> e {light = Just v}) <$> quoted floatLiteral
  "lip"         -> (\v e -> e {lip = Just v}) <$> quoted floatLiteral
  "height"      -> (\v e -> e {height = Just v}) <$> quoted floatLiteral
  "phase"       -> (\v e -> e {phase = Just v}) <$> quoted floatLiteral
  "delay"       -> (\v e -> e {delay = Just v}) <$> quoted floatLiteral

  "color"       -> (\v e -> e {color = Just v}) <$> quoted vector3

  "count"       -> (\v e -> e {count = Just v}) <$> quoted integerLiteral
  "dmg"         -> (\v e -> e {damage = Just v}) <$> quoted integerLiteral
  "nobots"      -> (\v e -> e {nobots = Just v}) <$> quoted integerLiteral
  "nohumans"    -> (\v e -> e {nohumans = Just v}) <$> quoted integerLiteral
  "health"      -> (\v e -> e {health = Just v}) <$> quoted integerLiteral
  "noglobalsound" -> (\v e -> e {noglobalsound = Just v}) <$> quoted integerLiteral

  _ -> return id

data EntityData
  = EntityData
  { classname     :: String
  , spawnflags    :: Int
  , origin        :: Vec3
  , angles        :: Vec3
  , notsingle     :: Bool
  , notteam       :: Bool
  , notfree       :: Bool
  , notq3a        :: Bool
  , speed         :: Maybe Float
  , wait          :: Maybe Float
  , random        :: Maybe Float
  , gravity       :: Maybe Float
  , roll          :: Maybe Float
  , light         :: Maybe Float
  , lip           :: Maybe Float
  , height        :: Maybe Float
  , phase         :: Maybe Float
  , delay         :: Maybe Float
  , color         :: Maybe Vec3
  , count         :: Maybe Int
  , damage        :: Maybe Int
  , nobots        :: Maybe Int
  , nohumans      :: Maybe Int
  , health        :: Maybe Int
  , noglobalsound :: Maybe Int
  , model         :: Maybe String
  , model2        :: Maybe String
  , target        :: Maybe String
  , targetname    :: Maybe String
  , team          :: Maybe String
  , gametype      :: Maybe String
  , message       :: Maybe String
  , noise         :: Maybe String
  , music         :: Maybe String
  , targetShaderName    :: Maybe String
  , targetShaderNewName :: Maybe String
  }
  deriving Show

defaultEntityData = EntityData
  { classname     = ""
  , spawnflags    = 0
  , origin        = zero
  , angles        = zero
  , notsingle     = False
  , notteam       = False
  , notfree       = False
  , notq3a        = False
  , speed         = Nothing
  , wait          = Nothing
  , random        = Nothing
  , gravity       = Nothing
  , roll          = Nothing
  , light         = Nothing
  , lip           = Nothing
  , height        = Nothing
  , phase         = Nothing
  , delay         = Nothing
  , color         = Nothing
  , count         = Nothing
  , damage        = Nothing
  , nobots        = Nothing
  , nohumans      = Nothing
  , health        = Nothing
  , noglobalsound = Nothing
  , model         = Nothing
  , model2        = Nothing
  , target        = Nothing
  , targetname    = Nothing
  , team          = Nothing
  , gametype      = Nothing
  , message       = Nothing
  , noise         = Nothing
  , music         = Nothing
  , targetShaderName    = Nothing
  , targetShaderNewName = Nothing
  }
