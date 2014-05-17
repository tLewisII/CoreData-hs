-- | Main entry point to the application.
module Main where
import CoreDataHs
import QueryBuild
import System.Environment
import Control.Arrow

progressIO :: String -> IO ()
progressIO = putStrLn . ("Creating file " ++)

main :: IO ()
main = do
  args <- getArgs
  let modelName = if not (null args) then head args else error "no Model name given."
  entities <- fmap modelEntities $ readFile $ fullModelPath modelName
  sequence_ [progressIO p >> writeFile p c | (p, c) <- map (intFileName &&& buildDeclaration) entities]
  sequence_ [progressIO p >> writeFile p c | (p, c) <- map (impFileName &&& buildImplementation) entities]
  putStrLn "done!"
