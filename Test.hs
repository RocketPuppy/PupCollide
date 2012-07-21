module Main where

import PupEventsClient as Client
import PupEventsPQueue
import Control.Concurrent.STM
import Events
import Control.Monad
import Text.Parsec
import HandlersClient
import Control.Concurrent

main =
    do  (q1, q2) <- doClient Nothing 3
        forkIO $ recvEvents q2
        forever $ sendMessage q1

sendMessage queue =
    do  putStrLn $ "Enter username:"
        message <- getLine
        let event = case message of
                        "lo" -> Logout
                        xs -> Login $ Username xs
        atomically $ writeThing queue (lookupPriority event) event

recvEvents queue = forever $
    do  event <- atomically $
            do  e <- getThing queue
                case e of
                    Nothing -> retry
                    Just event -> return event
        (lookupHandlerClient event) event