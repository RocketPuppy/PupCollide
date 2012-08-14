module TestServer where

import Test.HUnit
import PupEventsPQueue
import Events
import qualified PupEventsClient as Client
import qualified Server as Server
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception.Base
import Data.List
import System.Environment
import qualified Distribution.TestSuite as Cabal
import qualified Distribution.TestSuite.HUnit as CabalHUnit

tests = map (\(x,y) -> CabalHUnit.test x y) 
    [ ("Login tests", loginTests)
    ]

-- Start server
startServer =
    do  stuff <- withArgs ["localhost", "3"] Server.mainTest
        return stuff

-- Make connection
connect =
    do  (q1, q2, disconnect) <- Client.client Nothing 3 lookupPriority lookupUnHandler parsers
        return (q1, q2, disconnect)

-- Bracketing
bracketed = bracket
    (
    do  (g, u, pI, r) <- startServer
        tId <- forkIO r
        (q1, q2, dc) <- connect
        return (g, u, pI, q1, q2, dc, tId)
    )
    (\(_,_,_,_,_,dc, tId) ->
        do  dc
            threadDelay 1000
            killThread tId
        )
    --(startServer >>= (\(g, u, pI, r) -> forkIO r >>= (\tId -> threadDelay 100 >> connect >>= (\(q1, q2, dc) -> return (g, u, pI, q1, q2, dc, tId)))))
    --(\(_,_,_,_,_,dc,tId) -> dc >> killThread tId)


-- Just for convenience
getEvent q = atomically $ 
    do  e <- getThing q
        case e of
            Nothing -> retry
            Just event -> return event

-- Custom assertions
-- |assertion to test if something is found in a list with a custom predicate
assertFound msg p xs =
    case find p xs of
        Nothing -> assertFailure msg
        Just _ -> return ()

-- |assertion to test if something is not found in a list with a custom predicate
assertNotFound msg p xs =
    case find p xs of
        Nothing -> return ()
        Just _ -> assertFailure msg

-- |assertion to test if a list is empty
assertEmpty msg xs
    | not (null xs) = assertFailure msg
    | otherwise = return ()

-- |assertion to test if a list is not empty
assertNotEmpty msg xs
    | null xs = assertFailure msg
    | otherwise = return ()

-- Login tests
loginTests = TestLabel "Login Tests" $
    TestList 
        [ TestLabel "Login !Exist !Registered" (TestCase loginNoExistNoRegister)
        , TestLabel "Login !Exist Registered" (TestCase loginNoExistRegister)]

-- |User doesn't exist and we haven't logged in yet.
loginNoExistNoRegister = bracketed $ \(g, u, pI, iQ, oQ, _, _) ->
    do  let user = Username "user1"
        let event = Login user
        -- get initial values
        users <- readTVarIO u
        players <- readTVarIO pI
        let usersLen = length users
        let playersLen = length players
        -- before login
        assertEmpty "Initial usernames not empty!" users
        assertEmpty "Initial playerInfos not empty!" players
        assertNotFound "User already exists!" ((==) user) users
        assertNotFound "Player already exists!" (\(_, x, _) -> x == user) players
        -- send event and get response
        atomically $ writeThing iQ (lookupPriority event) event
        event' <- getEvent oQ
        -- get updated values
        users' <- readTVarIO u
        players' <- readTVarIO pI
        let usersLen' = length users'
        let playersLen' = length players'
        --after login
        assertEqual "Usernames not increased by 1" (usersLen + 1) (usersLen')
        assertEqual "PlayerInfos not increased by 1" (playersLen + 1) (playersLen')
        assertFound "User not found!" ((==) user) users'
        assertFound "Player not found!" (\(_, x, _) -> x == user) players'
        -- check that we have the expected event
        assertEqual "Not Login event!" event event'

-- |User doesn't exist and we've logged in already
loginNoExistRegister = bracketed $ \(g, u, pI, iQ, oQ, _, _) ->
    do  let user = Username "user1"
        let login = Login user
        atomically $ writeThing iQ (lookupPriority login) login
        login' <- getEvent oQ
        -- make sure we're actually Logged In
        assertEqual "User Exists" login login'
        -- initial data before we do the actual test
        users <- readTVarIO u
        players <- readTVarIO pI
        let usersLen = length users
        let playersLen = length players
        let user2 = Username "user2"
        let login2 = Login user
        -- the following must be true for the initial data to be correct
        assertNotEmpty "Initial users is empty!" users
        assertNotEmpty "Initial playerInfos is empty!" players
        assertNotFound "User already exists!" ((==) user2) users
        assertNotFound "Player already exists!" (\(_, x, _) -> x == user2) players
        atomically $ writeThing iQ (lookupPriority login2) login2
        login2' <- getEvent oQ
        users' <- readTVarIO u
        players' <- readTVarIO pI
        assertEqual "Users have changed!" users users'
        assertEqual "Players have changed!" players players'
        assertEqual "Did not receive proper event!" login2' (Error AlreadyRegistered)
