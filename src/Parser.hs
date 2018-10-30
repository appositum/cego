module Parser where

import Control.Applicative
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

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  pure (SemVer major minor patch [] [])
