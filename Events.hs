{-# LANGUAGE DeriveDataTypeable#-}
module Events where

import Text.Parsec
import qualified PupEventsServer as Server
import System.Environment
import qualified Data.Time.Clock as Time
import Text.JSON.Generic
import qualified Text.JSON.Parsec as JSONP
import qualified Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

-- |Put your event models here
data Event  = MouseMove Float Float
            | Login Username
            | Logout
            | GameNew [Username]
            | GameJoin Username Game
            | GameLeave Username Game
            | GameStart Game
            | GameAdvance Game
            | GameEnd Game
            | GamesRequest
            | GamesList [Game]
            | PlayerCollision Game [Username]
            | UserExists Username
            | AlreadyRegistered
    deriving (Typeable, Data)

data Game =
    Game    { gameUuid :: UUID
            , gameAlive :: Maybe [Player]
            , gameDead :: Maybe [Player]
            , gameStatus :: Maybe GameStatus
            , gameDifficulty :: Maybe Integer
            , gameStarted :: Maybe Time.UTCTime
            , gameEnded :: Maybe Time.UTCTime
            }
    deriving (Typeable, Data)
instance Show Game where
    show (Game (UUID uuid) _ _ _ _ _ _) = uuid
data GameStatus = Created | InProgress | Ended
    deriving (Typeable, Data)
data Player =
    Player  { playerUsername :: Username
            , playerPosition :: Maybe (Float, Float)
            }
    deriving (Eq, Typeable, Data)
instance Show Player where
    show (Player (Username user) _) = user
newtype Username = Username String
    deriving (Show, Eq, Typeable, Data)
newtype UUID = UUID String
    deriving (Show, Eq, Typeable, Data)

---------------
-- GamesList --
---------------
gamesList :: Parsec String () Event
gamesList = 
    do  parsed <- JSONP.p_js_object
        let games = case fromJSON (JSObject parsed) of
                        Text.JSON.Ok x -> x
                        Text.JSON.Error m -> error m
        return games

unGamesList :: Event -> String
unGamesList e@(GamesList _ ) = "GamesList\0" ++ encodeJSON e ++ "\0\0"

------------------
-- GamesRequest --
------------------
gamesRequest :: Parsec String () Event
gamesRequest =
    do  string "GamesRequest"
        string "\0\0"
        return GamesRequest
unGamesRequest :: Event -> String
unGamesRequest GamesRequest = "GamesRequest" ++ "\0\0"
-----------------------
-- AlreadyRegistered --
-----------------------
alreadyRegistered :: Parsec String () Event
alreadyRegistered =
    do  string "AlreadyRegistered"
        string "\0\0"
        return AlreadyRegistered

unAlreadyRegistered :: Event -> String
unAlreadyRegistered AlreadyRegistered = "AlreadyRegistered" ++ "\0\0"
----------------
-- UserExists --
----------------
userExists :: Parsec String () Event
userExists =
    do  string "UserExists"
        char '\0'
        username <- many $ alphaNum
        string "\0\0"
        return (UserExists $ Username username)

unUserExists :: Event -> String
unUserExists (UserExists (Username user)) =
    "UserExists\0" ++ user ++ "\0\0"
---------------
-- GameLeave --
---------------
gameLeave :: Parsec String () Event
gameLeave =
    do  string "GameLeave"
        char '\0'
        player <- manyTill alphaNum (char '\0')
        game <- manyTill (alphaNum <|> char '-') (string "\0\0") 
        return $ GameLeave (Username player) 
                    (Game (UUID game) Nothing Nothing Nothing Nothing Nothing Nothing)

unGameLeave :: Event -> String
unGameLeave (GameLeave player game) =
    "GameLeave\0" ++ show player ++ show game ++ "\0\0"
---------------------
-- PlayerCollision --
---------------------
playerCollision :: Parsec String () Event
playerCollision =
    do  string "PlayerCollision"
        char '\0'
        game <- manyTill (alphaNum <|> char '-') (char '\0')
        players <- manyTill (manyTill (char '\0') alphaNum) (string "\0\0")
        return $ PlayerCollision (Game (UUID game) Nothing Nothing Nothing Nothing Nothing Nothing) $ map (Username) players

unPlayerCollision :: Event -> String
unPlayerCollision (PlayerCollision game players) =
    "PlayerCollision\0" ++ show game ++ "\0" ++
        concatMap ((flip (++)) "\0" . show) players ++ "\0\0"
-------------
-- GameEnd --
-------------
gameEnd :: Parsec String () Event
gameEnd =
    do  string "GameEnd"
        char '\0'
        game <- manyTill (alphaNum <|> char '-') (string "\0\0")
        return $ GameEnd (Game (UUID game) Nothing Nothing Nothing Nothing Nothing Nothing)

unGameEnd :: Event -> String
unGameEnd (GameEnd game) =
    "GameEnd\0" ++ show game ++ "\0\0"
-----------------
-- GameAdvance --
-----------------
gameAdvance :: Parsec String () Event
gameAdvance =
    do  string "GameAdvance"
        char '\0'
        game <- manyTill (alphaNum <|> char '-') (string "\0\0")
        return $ GameAdvance (Game (UUID game) Nothing Nothing Nothing Nothing Nothing Nothing)

unGameAdvance :: Event -> String
unGameAdvance (GameAdvance game) =
    "GameAdvance\0" ++ show game ++ "\0\0"
---------------
-- GameStart --
---------------
gameStart :: Parsec String () Event
gameStart =
    do  string "GameStart"
        char '\0'
        game <- manyTill (alphaNum <|> char '-') (string "\0\0")
        return $ GameStart (Game (UUID game) Nothing Nothing Nothing Nothing Nothing Nothing)

unGameStart :: Event -> String
unGameStart (GameStart game) =
    "GameStart\0" ++ show game ++ "\0\0"
--------------
-- GameJoin --
--------------
gameJoin :: Parsec String () Event
gameJoin =
    do  string "GameJoin"
        char '\0'
        player <- manyTill alphaNum (char '\0')
        game <- manyTill (alphaNum <|> char '-') (string "\0\0")
        return $ GameJoin (Username player) (Game (UUID game) Nothing Nothing Nothing Nothing Nothing Nothing)

unGameJoin :: Event -> String
unGameJoin (GameJoin player game) =
    "GameJoin\0" ++ show player ++ show game ++ "\0\0"
-------------
-- GameNew --
-------------
gameNew :: Parsec String () Event
gameNew =
    do  string "GameNew"
        char '\0'
        players <- manyTill (manyTill (char '\0') alphaNum) (string "\0\0")
        return $ GameNew $ map (Username) players

unGameNew :: Event -> String
unGameNew (GameNew players) =
    "GameNew\0" ++ concatMap ((flip (++)) "\0" . show) players ++ "\0\0"
---------------
-- MouseMove --
---------------
mouseMove :: Parsec String () Event
mouseMove =
    do  string "MouseMove"
        char '\0'
        p1 <- many $ oneOf "0123456789-+e."
        char '\0'
        p2 <- many $ oneOf "0123456789-+e."
        string "\0\0"
        return (MouseMove (read p1) (read p2))

unMouseMove :: Event -> String
unMouseMove (MouseMove p1 p2) =
    "MouseMove\0" ++ (show p1) ++ '\0':(show p2) ++ "\0\0"

-----------
-- Login --
-----------
login :: Parsec String () Event
login =
    do  string "Login"
        char '\0'
        username <- many $ alphaNum
        string "\0\0"
        return (Login $ Username username)

unLogin :: Event -> String
unLogin (Login (Username user)) =
    "Login\0" ++ user ++ "\0\0"

------------
-- Logout --
------------
logout :: Parsec String () Event
logout =
    do  string "Logout"
        string "\0\0"
        return (Logout)

unLogout :: Event -> String
unLogout (Logout) =
    "Logout" ++ "\0\0"

-- |This is a list of the event parsers to be used when trying to parse messages. If you don't add your event parsers to this list they won't get called!
parsers =   map try [mouseMove, login, logout, gameNew, gameJoin, gameStart
                    , gameAdvance , gameEnd, playerCollision
                    , alreadyRegistered, userExists, gamesList
                    , gamesRequest]

-- |Returns the specified Event's priority level
lookupPriority :: Event -> Int
lookupPriority (MouseMove _ _ ) = 0
lookupPriority (Login _ ) = 2
lookupPriority (Logout ) = 2
lookupPriority (GameNew _ ) = 2
lookupPriority (GameJoin _ _ ) = 1
lookupPriority (GameLeave _ _ ) = 1
lookupPriority (GameStart _ ) = 2
lookupPriority (GameAdvance _ ) = 0
lookupPriority (GameEnd _ ) = 1
lookupPriority (PlayerCollision _ _ ) = 0
lookupPriority (UserExists _ ) = 2
lookupPriority (AlreadyRegistered) = 2
lookupPriority (GamesRequest) = 2
lookupPriority (GamesList _ ) = 2

-- |Returns the function used to "unhandle" an Event, that is convert
-- it to a string.
lookupUnHandler :: Event -> (Event -> String)
lookupUnHandler (MouseMove _ _ ) = unMouseMove
lookupUnHandler (Login _ ) = unLogin
lookupUnHandler (Logout ) = unLogout
lookupUnHandler (GameNew _ ) = unGameNew
lookupUnHandler (GameJoin _ _ ) = unGameJoin
lookupUnHandler (GameLeave _ _ ) = unGameLeave
lookupUnHandler (GameStart _ ) = unGameStart
lookupUnHandler (GameAdvance _ ) = unGameAdvance
lookupUnHandler (GameEnd _ ) = unGameEnd
lookupUnHandler (PlayerCollision _ _ ) = unPlayerCollision
lookupUnHandler (AlreadyRegistered) = unAlreadyRegistered
lookupUnHandler (UserExists _ ) = unUserExists
lookupUnHandler (GamesRequest) = unGamesRequest
lookupUnHandler (GamesList _ ) = unGamesList