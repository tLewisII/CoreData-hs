-- | Main entry point to the application.
module Main where
import CoreDataHs
import QueryBuild
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let modelName = if (length args) > 0 then (head args) else error "no Model name given."
  xml <- readFile (fullModelPath modelName)
  let a = modelEntities xml
  sequence_ [(writeFile p c) | (p, c) <- zip (map intFileName a) (map buildDeclaration a)]
  sequence_ [(writeFile p c) | (p, c) <- zip (map impFileName a) (map buildImplementation a)]
