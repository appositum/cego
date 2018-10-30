module Parser where

import Control.Applicative
import Control.Monad (join)
import Data.Maybe (maybeToList)
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer
  deriving (Eq, Show)
          
type Major = Integer
type Minor = Integer                            
type Patch = Integer                       
type Release = [NumberOrString]              
type Metadata = [NumberOrString]

data SemVer = SemVer
  { getMajor    :: Major
  , getMinor    :: Minor
  , getPatch    :: Patch
  , getRelease  :: Release
  , getMetadata :: Metadata
  } deriving (Eq, Show)

alpha :: Parser Char
alpha = oneOf $ ['a'..'z'] ++ ['A'..'Z']

skipDots :: Parser ()
skipDots = skipMany (char '.')

numOrStr :: Parser NumberOrString
numOrStr = do
  skipDots
  val <- NOSI <$> integer <|> NOSS <$> (some alpha)
  skipDots
  pure val

parseRelease :: Parser Release
parseRelease = do
  pre <- optional $
    string "alpha.beta"
    <|> string "alpha"
    <|> string "beta"
    <|> string "rc"
  list <- many numOrStr
  case pre of
    Nothing -> pure list
    Just p  -> pure (NOSS p : list)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  release <- optional $ char '-' *> parseRelease
  let release' = join $ maybeToList release
  pure (SemVer major minor patch release' [])
