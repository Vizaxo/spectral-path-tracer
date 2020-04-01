module Main where

import System.Environment

import PathTracer.Loop

main :: IO ()
main = getArgs >>= \case
  [path] -> pathTracer path
  _ -> do
    name <- getProgName
    putStrLn ("Usage: " ++ name ++ " path_to_shader_directory")
