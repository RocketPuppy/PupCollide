module Server (
    main,
    mainTest,
    lookupHandlerServer
    ) where

import Text.Parsec hiding (Error)
import qualified PupEventsServer as Server
import System.Environment
import System.Random
import Events
import Control.Concurrent.STM
import qualified Data.UUID as UUID2
import qualified Data.UUID.V4 as UUIDV4
import Data.List
import Control.Concurrent
import System.Random
import Data.Maybe

-- |Main entry point. You should pass in the address to connect to and
-- the number of priority levels desired.
main :: IO ()
main =
    do  args <- getArgs
        let ip = args !! 0
        let priorities = read (args !! 1) :: Int
        games <- newTVarIO [] :: IO (TVar [Game])
        usernames <- newTVarIO [] :: IO (TVar [Username])
        playerInfos <- newTVarIO [] :: IO (TVar [(ThreadId, Username, Maybe Game)])
        Server.server (Just ip) priorities "1267" lookupPriority lookupUnHandler (lookupHandlerServer games usernames playerInfos) parsers (Just (Logout (Username "")))

-- | Main method used for testing, it returns the state variables and an IO action to actually start the server.
mainTest :: IO (TVar [Game], TVar [Username], TVar [(ThreadId, Username, Maybe Game)], IO b)
mainTest =
    do  args <- getArgs
        let ip = args !! 0
        let priorities = read (args !! 1) :: Int
        games <- newTVarIO [] :: IO (TVar [Game])
        usernames <- newTVarIO [] :: IO (TVar [Username])
        playerInfos <- newTVarIO [] :: IO (TVar [(ThreadId, Username, Maybe Game)])
        port <- randomRIO (1024, 65535)
        let runnable = Server.server (Just ip) priorities (show port) lookupPriority lookupUnHandler (lookupHandlerServer games usernames playerInfos) parsers (Just (Logout (Username "")))
        return (games, usernames, playerInfos, runnable)
------------------
-- GamesRequest --
------------------
--gameRequestHandlerServer :: TVar [(Game)] -> Event -> IO Event
--gameRequestHandlerServer games e@(GamesRequest) = 
--    do  gamelist <- readTVarIO games
--        return (GamesList gamelist)
-----------------
---- GamesList --
-----------------
--gamesListHandlerServer :: TVar [(Game)] -> Event -> IO Event
--gamesListHandlerServer games e@(GamesList list) = return e
-----------------
---- GameLeave --
-----------------
--gameLeaveHandlerServer :: TVar [(ThreadId, Player)] -> TVar [(Game)] -> Event -> IO Event
--gameLeaveHandlerServer usernames games e@(GameLeave player game) =
--    do  usernames' <- readTVarIO usernames
--        games' <- readTVarIO games
--        let pExists = find ((==) player . playerUsername . snd) usernames'
--        let gExists = find ((==) game) games'
--        if and $ map isJust [pExists, gExists]
--            then atomically $ modifyTVar games $ removePlayerFromGame (fromJust pExists) (fromJust gExists)
--            else return e -- TODO Unable to join game!
--        return e
--    where
--        removePlayerFromGame player game games =
--            (:) (newgame game) (delete game games)
--            where 
--                newgame game' =
--                    Game    { gameUuid = (gameUuid game')
--                            , gameAlive = delete player (gameAlive game')
--                            , gameDead = gameDead game'
--                            , gameStatus = gameStatus game'
--                            , gameDifficulty = gameDifficulty game'
--                            , gameStarted = gameStarted game'
--                            , gameEnded = gameEnded game'
--                            }
-----------------------
---- PlayerCollision --
-----------------------
--playerCollisionHandlerServer :: TVar [(Game)] -> Event -> IO Event
--playerCollisionHandlerServer games e@(PlayerCollision game usernames) = return e
---------------
---- GameEnd --
---------------
--gameEndHandlerServer :: TVar [(Game)] -> Event -> IO Event
--gameEndHandlerServer games e@(GameEnd game) = 
--    do  games' <- readTVarIO games
--        let gExists = find ((==) game . gameUuid) games'
--        if isJust gExists
--            then atomically $ modifyTVar games $ endGame (fromJust gExists)
--            else return e -- TODO Unable to end game!
--        return e
--    where
--        endGame game games =
--            (:) (ended game) (delete game games)
--            where
--                ended game' =
--                    Game    { gameUuid = (gameUuid game')
--                            , gameAlive = (gameAlive game')
--                            , gameDead = gameDead game'
--                            , gameStatus = Ended
--                            , gameDifficulty = gameDifficulty game'
--                            , gameStarted = gameStarted game'
--                            , gameEnded = gameEnded game'
--                            }
-------------------
---- GameAdvance --
-------------------
--gameAdvanceHandlerServer :: TVar [(Game)] -> Event -> IO Event
--gameAdvanceHandlerServer games e@(GameAdvance game) = return e
-----------------
---- GameStart --
-----------------
--gameStartHandlerServer :: TVar [(Game)] -> Event -> IO Event
--gameStartHandlerServer games e@(GameStart game) = 
--    do  games' <- readTVarIO games
--        let gExists = find ((==) game . gameUuid) games'
--        if isJust gExists
--            then atomically $ modifyTVar games $ startGame (fromJust gExists)
--            else return e -- TODO Unable to start game!
--        return e
--    where
--        startGame game games =
--            (:) (started game) (delete game games)
--            where
--                started game' =
--                    Game    { gameUuid = (gameUuid game')
--                            , gameAlive = (gameAlive game')
--                            , gameDead = gameDead game'
--                            , gameStatus = InProgress
--                            , gameDifficulty = gameDifficulty game'
--                            , gameStarted = gameStarted game'
--                            , gameEnded = gameEnded game'
--                            }
----------------
---- GameJoin --
----------------
---- make sure game/player exists, then add player to game's alive/dead list
--gameJoinHandlerServer :: TVar [(ThreadId, Player)] -> TVar [(Game)] -> Event -> IO Event
--gameJoinHandlerServer usernames games e@(GameJoin player (UUID game)) = 
--    do  usernames' <- readTVarIO usernames
--        games' <- readTVarIO games
--        let pExists = find ((==) player . playerUsername) usernames'
--        let gExists = find ((==) game . gameUuid) games'
--        if and $ map isJust [pExists, gExists]
--            then atomically $ modifyTVar games $ addPlayerToGame (fromJust pExists) (fromJust gExists)
--            else return e -- TODO Unable to join game!
--        return e
--    where
--        addPlayerToGame player game games =
--            (:) (newgame game) (delete game games)
--            where 
--                newgame game' =
--                    Game    { gameUuid = (gameUuid game')
--                            , gameAlive = (setRandomPosition player):(gameAlive game')
--                            , gameDead = gameDead game'
--                            , gameStatus = gameStatus game'
--                            , gameDifficulty = gameDifficulty game'
--                            , gameStarted = gameStarted game'
--                            , gameEnded = gameEnded game'
--                            }

---------------
---- GameNew --
---------------
--gameNewHandlerServer :: TVar [(Game)] -> Event -> IO Event
--gameNewHandlerServer games e@(GameNew usernames) =
--    do  x <- UUIDV4.nextRandom
--        let uuid = UUID2.toString x
--        let usernames' = mapM (setRandomPosition) usernames
--        let game = Game { gameUuid = (UUID uuid)
--                        , gameAlive = Just usernames'
--                        , gameDead = Just []
--                        , gameStatus = Just Created
--                        , gameDifficulty = Just 0
--                        , gameStarted = Nothing
--                        , gameEnded = Nothing
--                        }
--        atomically $ modifyTVar games ((:) game)
--        return e
-----------------
---- MouseMove --
-----------------
--mouseMoveHandlerServer :: TVar [(Game)] -> Event -> IO Event
--mouseMoveHandlerServer games e@(MouseMove p1 p2) = return e
-----------
-- Login --
-----------
-- | Check if the requested username exists or if the user has already registered ( previous registration is signaled by this thread being in the list of playerInfos). If both of those are false than we log the user in.
loginHandlerServer :: TVar [Username] -> TVar [(ThreadId, Username, Maybe Game)] -> Event -> IO Event
loginHandlerServer usernames playerInfos e@(Login user) = 
    do  threadid <- myThreadId
        -- we do this in one atomically block so we can't have the available usernames/playerInfos change on us.
        atomically $
            do  usernames' <- readTVar usernames
                playerInfos' <- readTVar playerInfos
                -- if any of the conditions are true than the user can't log in
                if or [snd c | c <- conditions threadid usernames' playerInfos']
                    then
                        -- this finds the first error code that has a True condition. The use only gets one error code so he can get rid of them one by one if there are multiple errors
                        return $ maybe (e) (Error . fst) (find ((==) True . snd) (conditions threadid usernames' playerInfos'))
                    else
                        -- add the user to the list of usernames and the (threadid, user, Nothing) to the playerinfos.
                        do  modifyTVar usernames (\x -> user:x)
                            modifyTVar playerInfos (\x -> (threadid, user, Nothing):x)
                            return e
    where
        -- conditions, we return the error code which corresponds to a True value as well as whether or not the condition is satisfied
        conditions t u p= [(UserExists, userExists u), (AlreadyRegistered, alreadyRegistered t p)]
        -- check if the user is in a list of users
        userExists = elem user
        -- check if this threadid is in a list that has threadids
        alreadyRegistered t = isJust . (find (\(x, _, _) -> x == t))

------------
-- Logout --
------------
-- | Check if the user is currently logged in, and if he is log him out.
logoutHandlerServer :: TVar [Username] -> TVar [(ThreadId, Username, Maybe Game)] -> Event -> IO Event
logoutHandlerServer usernames playerInfos e@(Logout _) =
    do  threadid <- myThreadId
        -- we do this in one atomically block so we can't have the available usernames/playerInfos change on us.
        atomically $
            do  usernames' <- readTVar usernames
                playerInfos' <- readTVar playerInfos
                -- attempt to find ourself in the playerInfos list
                let pInfo = find (\(x, _, _) -> x == threadid) playerInfos'
                -- If we found something then we're logged in, log us out, if not return the NotLoggedIn Error.
                case pInfo of
                    Just pI@(_, user, _) ->
                        do  modifyTVar usernames (delete user)
                            -- we need to use our own predicate to delete a playerInfo
                            modifyTVar playerInfos (deleteBy (\(t, _, _) (t', _, _) -> t == t') pI)
                            return e
                    Nothing -> return $ Error NotLoggedIn

-- |Returns the specified Event's handler function. This has a weird type signature because it's returning a function.
lookupHandlerServer ::  TVar [Game]
                        -> TVar [Username]
                        -> TVar [(ThreadId, Username, Maybe Game)]
                        -> Event -> (Event -> IO Event)
--lookupHandlerServer _ games (MouseMove _ _ ) = mouseMoveHandlerServer games
lookupHandlerServer games usernames playerInfos (Login _ ) =
    loginHandlerServer usernames playerInfos
lookupHandlerServer games usernames playerInfos (Logout _ ) =
    logoutHandlerServer usernames playerInfos
--lookupHandlerServer _ games (GameNew _ ) = gameNewHandlerServer games
--lookupHandlerServer usernames games (GameJoin _ _ ) = gameJoinHandlerServer usernames games
--lookupHandlerServer usernames games (GameLeave _ _ ) = gameLeaveHandlerServer usernames games
--lookupHandlerServer _ games (GameStart _ ) = gameStartHandlerServer games
--lookupHandlerServer _ games (GameAdvance _ ) = gameAdvanceHandlerServer games
--lookupHandlerServer _ games (GameEnd _ ) = gameEndHandlerServer games
--lookupHandlerServer _ games (PlayerCollision _ _ ) = playerCollisionHandlerServer games

---- Helper functions
--setRandomPosition (Player username _ )=
--    do  randX <- randomRIO ((-1.0), 1.0)
--        randY <- randomRIO ((-1.0), 1.0)
--        let randPos = (randX, randY)
--        return (Player username randPos)
