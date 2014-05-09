-- | Main entry point to the application.
module Main where
import CoreDataHs
import QueryBuild
import System.Environment
import Control.Arrow

main :: IO ()
main = do
  args <- getArgs
  let modelName = if not (null args) then head args else error "no Model name given."
  xml <- readFile (fullModelPath modelName)
  let a = modelEntities xml
  sequence_ [writeFile p c | (p, c) <- map (intFileName &&& buildDeclaration) a]
  sequence_ [writeFile p c | (p, c) <- map (impFileName &&& buildImplementation) a]
