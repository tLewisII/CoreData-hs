-- | Main entry point to the application.
module Main where
import CoreDataHs
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let modelName   = if (length args) > 0 then (head args) else error "no Model name given."
  xml <- readFile (fullModelPath modelName)
  print $ modelEntities xml
