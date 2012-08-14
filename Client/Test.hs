module Main where

import PupEventsClient as Client
import PupEventsPQueue
import Control.Concurrent.STM
import Events
import Control.Monad
import Text.Parsec
import HandlersClient
import Control.Concurrent
import Control.Exception.Base (finally)

main =
    do  (q1, q2, dc) <- doClient Nothing 3
        forkIO $ recvEvents q2
        finally (forever $ sendMessage q1) (putStrLn "closing..." >> dc)

sendMessage queue =
    do  putStrLn $ "Enter username:"
        message <- getLine
        let event = case message of
                        "lo" -> Logout $ Username ""
                        xs -> Login $ Username xs
        atomically $ writeThing queue (lookupPriority event) event

recvEvents queue = forever $
    do  event <- atomically $
            do  e <- getThing queue
                case e of
                    Nothing -> retry
                    Just event -> return event
        (lookupHandlerClient event) event