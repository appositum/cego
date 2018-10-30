module Main where

import Control.Applicative
import Parser
import Text.Trifecta

main :: IO ()
main = pure ()

parse :: Parser a -> String -> Result a
parse p = parseString p mempty
