-- | Main entry point to the application.
module Main where
import CoreDataHs
import QueryBuild
import System.Environment
import Control.Arrow
import Control.Applicative
import Data.Maybe

version :: IO ()
version = putStrLn "coredata-hs version 0.1.0"

progressIO :: String -> IO ()
progressIO = putStrLn . ("Creating file " ++)

ifEntity :: String -> Maybe Entity -> IO [Entity]
ifEntity s Nothing = error ("No entity named " ++ "'" ++ s ++ "''" ++ " exists")
ifEntity _ e = return . maybeToList $ e


parseArgs :: [String] -> IO ()
parseArgs [] = error "no Model name given."
parseArgs ["-V"] = version
parseArgs ["--version"] = version
parseArgs [a] = modelEntities <$> readFile (fullModelPath a) >>= writeFiles
parseArgs [_, "--entity"] = error "No Entity name given"
parseArgs [a, "--entity", c] = modelEntities <$> readFile (fullModelPath a) >>= ifEntity c . findEntity c >>= writeFiles
parseArgs _ = putStrLn "Unrecognized command"


writeFiles :: [Entity] -> IO ()
writeFiles e = do
              sequence_ [progressIO p >> writeFile p c | (p, c) <- map (intFileName &&& buildDeclaration) e]
              sequence_ [progressIO p >> writeFile p c | (p, c) <- map (impFileName &&& buildImplementation) e]
              putStrLn "done!"

main :: IO ()
main = getArgs >>= parseArgs
