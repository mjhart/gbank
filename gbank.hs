module Gbank where

import Text.Read
import qualified Data.Map as Map

type Name = String
type Balance = Int
type Accounts = Map.Map String Int

main = do
  accounts <- setUp
  run accounts

run :: Accounts -> IO Accounts
run as = do
  putStrLn $ show as
  name <- getName
  putStrLn "Delta:"
  val <- getNumPlayers
  let as' = Map.adjust (+val) name as
  return as' >>= run

-- Set up stuff -- 
setUp :: IO Accounts
setUp = do
  putStrLn "Hi! Welcome to gbank. How many players will be playing today?"
  n <- getNumPlayers
  getAccounts n
 
getAccounts :: Int -> IO Accounts
getAccounts n = if n > 0
  then do
    name <- getName
    others <- getAccounts (n-1)
    return $ Map.insert name 0 others
  else return Map.empty
 
getName :: IO Name
getName = do
  putStrLn "Player name:"
  name <- getLine
  if null name
    then putStrLn "Name cannot be empty" >> getName
    else return name

getNumPlayers :: IO Int
getNumPlayers = do
  maybeNum <- getMaybeInt 
  case maybeNum of
    Just n -> return n
    Nothing -> putStrLn "Not a number. Please try again" >> getNumPlayers

getMaybeInt :: IO (Maybe Int)
getMaybeInt = getLine >>= return . readMaybe
