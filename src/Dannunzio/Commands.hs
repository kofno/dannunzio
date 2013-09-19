{-# LANGUAGE OverloadedStrings #-}
module Dannunzio.Commands where

import Text.ParserCombinators.Parsec
import Data.Digest.Pure.MD5
import Data.Char
import Control.Monad

type Mailbox = String
type MessageID = Integer

data Command = QUIT
             | NOOP
             | RSET
             | STAT
             | DELE MessageID
             | LIST [MessageID]
             | PASS String
             | RETR MessageID
             | TOP  MessageID Integer
             | UIDL [MessageID]
             | USER Mailbox
             | APOP Mailbox String
             deriving (Show)

parseCommand :: Parser (Maybe Command)
parseCommand = do
  c <- many1 (letter <?> "command")
  case map toLower c of
    "quit" -> return $ Just QUIT
    "noop" -> return $ Just NOOP
    "rset" -> return $ Just RSET
    "stat" -> return $ Just STAT
    "dele" -> liftM (Just . DELE) intArgument1
    "list" -> liftM (Just . LIST) manyIntArguments
    "pass" -> liftM (Just . PASS) passwordArgument
    "retr" -> liftM (Just . RETR) intArgument1
    "uidl" -> liftM (Just . UIDL) manyIntArguments
    "user" -> liftM (Just . USER) usernameArgument
    "top" -> do
      cmd <- liftM2 TOP intArgument1 intArgument1
      return (Just cmd)
    "apop" -> do
      cmd <- liftM2 APOP usernameArgument digestArgument
      return (Just cmd)
    _ -> return $ Nothing

-- <<helper parsers
intArgument1 :: Parser Integer
intArgument1 = do
  space
  ds <- many1 digit <?> "integer"
  return (read ds)

manyIntArguments :: Parser [Integer]
manyIntArguments = do
  space
  ds <- many1 digit `sepBy` space
  eof
  return (map read ds)

passwordArgument :: Parser String
passwordArgument = space >> many1 anyChar

usernameArgument :: Parser String
usernameArgument = space >> many1 (letter <|> (oneOf "@.+"))

digestArgument :: Parser String
digestArgument = space >> many1 anyChar
