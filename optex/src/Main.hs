module Main where

import Options.Applicative as Opt
import Data.Semigroup ((<>))
import Data.Char

-----------------------------------------------------------------------------------

data Welcome = Welcome { name :: String }

runWithOptions :: Welcome -> IO ()
runWithOptions opts =
  putStrLn $ "Enjoy the snow, " ++ name opts ++ "!"

-----------------------------------------------------------------------------------

main :: IO ()
main = do
--   execParser :: ParserInfo a -> IO a
  execParser opts >>= runWithOptions
  where
    parser = Welcome <$> argument str (metavar "NAME")
    opts = info parser mempty
--     info :: Parser a -> InfoMod a -> ParserInfo a

-----------------------------------------------------------------------------------
