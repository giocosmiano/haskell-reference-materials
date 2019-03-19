module Main where

import Options.Applicative
import Data.Monoid

-----------------------------------------------------------------------------------
-- |
-- Utilities and combinators for parsing command line options
-- https://github.com/pcapriotti/optparse-applicative
-- https://hackage.haskell.org/package/optparse-applicative

-----------------------------------------------------------------------------------

data Command = Add String String
             | Phone String String
             | Show String
             | Email String String
             deriving (Show, Eq)

parserAdd :: Parser Command
parserAdd = Add
  <$> strArgument (metavar "FILENAME")
  <*> strArgument (metavar "PERSON_NAME")

parserPhone :: Parser Command
parserPhone = Phone
  <$> strArgument (metavar "FILENAME")
  <*> strArgument (metavar "PHONE_NUMBER")

parserEmail :: Parser Command
parserEmail = Email
  <$> strArgument (metavar "FILENAME")
  <*> strArgument (metavar "EMAIL_ADDRESS")

parserShow :: Parser Command
parserShow = Show
  <$> strArgument (metavar "FILENAME")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parserCommand :: Parser Command
parserCommand = subparser $
    command "add" (parserAdd `withInfo` "Add an entry.") <>
    command "email" (parserEmail `withInfo` "Add email address.") <>
    command "phone" (parserPhone `withInfo` "Add phone number.") <>
    command "show" (parserShow `withInfo` "Show record.")

parserInfoCommand :: ParserInfo Command
parserInfoCommand = info parserCommand (progDesc "Manage address book.")

-----------------------------------------------------------------------------------

-- |
-- see https://github.com/hensmith/ppl
--
-- e.g.
-- $ stack exec -- address add gio "Gio Cosmiano"
-- Add "gio" "Gio Cosmiano"
--
-- $ stack exec -- address email gio "gio@email.com"
-- Email "gio" "gio@email.com"
--
-- $ stack exec -- address phone gio "123-456-7890"
-- Phone "gio" "123-456-7890"
--
-- $ stack exec -- address show gio
-- Show "gio"
--
-- $ stack exec -- address
-- Missing: COMMAND
-- Usage: address.EXE COMMAND
--   Manage address book.

main :: IO ()
main = do
  command <- execParser parserInfoCommand
  print command

-----------------------------------------------------------------------------------
