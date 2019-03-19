module Main where

import Options.Applicative as Opt
import Data.Semigroup ((<>))
import Data.Char (toUpper)

-----------------------------------------------------------------------------------
-- |
-- Utilities and combinators for parsing command line options
-- https://github.com/pcapriotti/optparse-applicative
-- https://hackage.haskell.org/package/optparse-applicative

-----------------------------------------------------------------------------------

data Welcome = Welcome { name :: String
                       , excited :: Bool }

runWithOptions :: Welcome -> IO ()
runWithOptions opts =
  putStrLn $ transform $ "Enjoy the snow, " ++ name opts ++ "!"
  where transform = if excited opts then map toUpper else id

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- $ stack exec -- optex Gio
-- Enjoy the snow, Gio!
--
-- $ stack exec -- optex Gio -e
-- ENJOY THE SNOW, GIO!
--
-- $ stack exec -- optex Gio --excited
-- ENJOY THE SNOW, GIO!
--
-- $ stack exec -- optex Gio -x
-- Invalid option `-x'
-- Usage: optex.EXE NAME [-e|--excited]

main :: IO ()
main = do
--   execParser :: ParserInfo a -> IO a
  execParser opts >>= runWithOptions
  where
    parser = Welcome <$> argument str (metavar "NAME")
                     <*> switch (short 'e' <>
                                long "excited" <>
                                help "Run in excited mode.")
    opts = info parser mempty
--     info :: Parser a -> InfoMod a -> ParserInfo a

-----------------------------------------------------------------------------------
