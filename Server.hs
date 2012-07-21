module Main  (
    main,
    lookupHandlerServer
    ) where

import Text.Parsec
import qualified PupEventsServer as Server
import System.Environment
import Events
import Control.Concurrent.STM
import qualified Data.UUID as UUID2
import qualified Data.UUID.V4 as UUIDV4
import Data.List
import Control.Concurrent
import System.Random

-- |Main entry point. You should pass in the address to connect to and
-- the number of priority levels desired.
main :: IO ()
main =
    do  args <- getArgs
        let ip = args !! 0
        let priorities = read (args !! 1) :: Int
        games <- newTVarIO [] :: IO (TVar [(Game, [Player])])
        players <- newTVarIO [] :: IO (TVar [(ThreadId, Player)])
        Server.server (Just ip) priorities lookupPriority lookupUnHandler (lookupHandlerServer players games) parsers

------------------
-- GamesRequest --
------------------
gameRequestHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gameRequestHandlerServer games e@(GamesRequest) = return e
---------------
-- GamesList --
---------------
gamesListHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gamesListHandlerServer games e@(GamesList e) = return e
---------------
-- GameLeave --
---------------
gameLeaveHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gameLeaveHandlerServer games e@(GameLeave player game) = return e
---------------------
-- PlayerCollision --
---------------------
playerCollisionHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
playerCollisionHandlerServer games e@(PlayerCollision game players) = return e
-------------
-- GameEnd --
-------------
gameEndHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gameEndHandlerServer games e@(GameEnd game) = return e
-----------------
-- GameAdvance --
-----------------
gameAdvanceHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gameAdvanceHandlerServer games e@(GameAdvance game) = return e
---------------
-- GameStart --
---------------
gameStartHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gameStartHandlerServer games e@(GameStart game) = return e
--------------
-- GameJoin --
--------------
gameJoinHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gameJoinHandlerServer games e@(GameJoin player game) = return e
-------------
-- GameNew --
-------------
gameNewHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
gameNewHandlerServer games e@(GameNew players) =
    do  x <- UUIDV4.nextRandom
        let uuid = UUID2.toString x
        randX <- randomRIO ((-1.0), 1.0)
        randY <- randomRIO ((-1.0), 1.0)
        let randPos = (randX, randY)
        let players' = map (flip Player $ (Just randPos)) players
        let game = Game { gameUuid = (UUID uuid)
                        , gameAlive = Just players'
                        , gameDead = Just []
                        , gameStatus = Just Created
                        , gameDifficulty = Just 0
                        , gameStarted = Nothing
                        , gameEnded = Nothing
                        }
        atomically $ modifyTVar games ((:) ((Game (UUID uuid) Nothing Nothing Nothing Nothing Nothing Nothing ), players'))
        return e
---------------
-- MouseMove --
---------------
mouseMoveHandlerServer :: TVar [(Game, [Player])] -> Event -> IO Event
mouseMoveHandlerServer games e@(MouseMove p1 p2) = return e
-----------
-- Login --
-----------
loginHandlerServer :: TVar [(ThreadId, Player)] -> Event -> IO Event
loginHandlerServer players e@(Login user) =
    do  putStrLn $ "Logging in user:" ++ show user
        let player = Player user Nothing
        threadid <- myThreadId
        players' <- readTVarIO players
        case lookup threadid players' of
            Nothing ->  case lookupUser user players' of
                            Nothing ->  do  addUser (threadid, player)
                                            showLoggedIn
                                            return e
                            Just _ -> return $ UserExists user
            Just _ ->   do  showLoggedIn
                            return AlreadyRegistered
    where 
        showLoggedIn =  do  players'' <- readTVarIO players
                            let loggedIn = [' ':show (playerUsername x) | (_, x) <- players'']
                            putStrLn $ "Users logged in:" ++ concat loggedIn
        addUser tp =    atomically $ modifyTVar players (\x -> tp:x)
        lookupUser user users = find (\(_, user2) -> (==) user (playerUsername user2)) users

------------
-- Logout --
------------
logoutHandlerServer :: TVar [(ThreadId, Player)] -> Event -> IO Event
logoutHandlerServer players e@(Logout) =
    do  threadid <- myThreadId
        atomically $ modifyTVar players (myDelete threadid)
        players'' <- readTVarIO players
        let loggedIn = [' ':show (playerUsername x) | (_, x) <- players'']
        putStrLn $ "Users logged in:" ++ concat loggedIn
        return e
    where   myDelete x y = fir ++ tail sec
                where (fir, sec) = break ((==) x . fst) y

-- |Returns the specified Event's handler function. This has a weird type signature because it's returning a function.
lookupHandlerServer :: TVar [(ThreadId, Player)]
                       -> TVar [(Game, [Player])] 
                       -> Event -> (Event -> IO Event)
lookupHandlerServer _ games (MouseMove _ _ ) = mouseMoveHandlerServer games
lookupHandlerServer players _ (Login _ ) = loginHandlerServer players
lookupHandlerServer players _ (Logout ) = logoutHandlerServer players
lookupHandlerServer _ games (GameNew _ ) = gameNewHandlerServer games
lookupHandlerServer _ games (GameJoin _ _ ) = gameJoinHandlerServer games
lookupHandlerServer _ games (GameLeave _ _ ) = gameLeaveHandlerServer games
lookupHandlerServer _ games (GameStart _ ) = gameStartHandlerServer games
lookupHandlerServer _ games (GameAdvance _ ) = gameAdvanceHandlerServer games
lookupHandlerServer _ games (GameEnd _ ) = gameEndHandlerServer games
lookupHandlerServer _ games (PlayerCollision _ _ ) = playerCollisionHandlerServer games
