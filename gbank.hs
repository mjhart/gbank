module Gbank where

import Text.Read

data Account = Account { name :: String, balance :: Int } deriving Show

main = do
  accounts <- setUp
  putStrLn $ show accounts
  run accounts

run :: [Account] -> IO ()
run as = do
  _ <- takeTurn as 0
  return ()

takeTurn :: [Account] -> Int -> IO [Account]
takeTurn as i = do
  putStrLn . name $ as !! i 
  _ <- getLine
  return takeTurn next $ (take (i-1) as) ++ [Account "blah" 5] ++ (drop i as)
    where next = i + 1 % 2

-- Set up stuff -- 
setUp :: IO [Account]
setUp = do
  putStrLn "Hi! Welcome to gbank. How many players will be playing today?"
  n <- getNumPlayers
  getAccountList n
 
getAccountList :: Int -> IO [Account]
getAccountList n = if n > 0
  then do
    account <- getAccount
    others <- getAccountList (n-1)
    return $ account : others
  else return []
 
getAccount :: IO Account
getAccount = do
  putStrLn "Player name:"
  name <- getLine
  if null name
    then putStrLn "Name cannot be empty" >> getAccount
    else return $ Account name 0

getNumPlayers :: IO Int
getNumPlayers = do
  maybeNum <- getMaybeInt 
  case maybeNum of
    Just n -> return n
    Nothing -> putStrLn "Not a number. Please try again" >> getNumPlayers

getMaybeInt :: IO (Maybe Int)
getMaybeInt = getLine >>= return . readMaybe
