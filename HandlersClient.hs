module HandlersClient (
    doClient,
    lookupHandlerClient
    )
where

import GHC.IO.Handle
import System.IO
import Control.Monad
import Control.Concurrent.STM.TChan
import Text.Parsec
import PupEventsPQueue
import qualified PupEventsClient as Client
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified PupEventsClient
import Events

-- |The doClient method is how we start the client side of the
-- Pup-Events framework. It's called by the main application.
doClient ip priorities =
    do  (q1, q2) <- Client.client ip priorities lookupPriority lookupUnHandler parsers
        return (q1, q2)

---------------
-- GameLeave --
---------------
gameLeaveHandlerClient :: Event -> IO Event
gameLeaveHandlerClient e@(GameLeave player game) = return e
---------------------
-- PlayerCollision --
---------------------
playerCollisionHandlerClient :: Event -> IO Event
playerCollisionHandlerClient e@(PlayerCollision game players) = return e
-------------
-- GameEnd --
-------------
gameEndHandlerClient :: Event -> IO Event
gameEndHandlerClient e@(GameEnd game) = return e
-----------------
-- GameAdvance --
-----------------
gameAdvanceHandlerClient :: Event -> IO Event
gameAdvanceHandlerClient e@(GameAdvance game) = return e
---------------
-- GameStart --
---------------
gameStartHandlerClient :: Event -> IO Event
gameStartHandlerClient e@(GameStart game) = return e
--------------
-- GameJoin --
--------------
gameJoinHandlerClient :: Event -> IO Event
gameJoinHandlerClient e@(GameJoin player game) = return e
-------------
-- GameNew --
-------------
gameNewHandlerClient :: Event -> IO Event
gameNewHandlerClient e@(GameNew players) = return e
---------------
-- MouseMove --
---------------
mouseMoveHandlerClient :: Event -> IO Event
mouseMoveHandlerClient e@(MouseMove p1 p2) = return e

-----------
-- Login --
-----------
loginHandlerClient :: Event -> IO Event
loginHandlerClient e@(Login (Username user)) = return e

------------
-- Logout --
------------
logoutHandlerClient :: Event -> IO Event
logoutHandlerClient e@(Logout) =
    do  putStrLn "Logged out"
        return e
-----------------------
-- AlreadyRegistered --
-----------------------
alreadyRegisteredHandlerClient :: Event -> IO Event
alreadyRegisteredHandlerClient e@(AlreadyRegistered) =
    do  putStrLn "Already registered with this IP!"
        return e

----------------
-- UserExists --
----------------
userExistsHandlerClient :: Event -> IO Event
userExistsHandlerClient e@(UserExists (Username user)) =
    do  putStrLn $ "The username " ++ show user ++ " already exists!"
        return e

-- |Returns the specified Event's handler function. This has a weird type signature because it's returning a function.
lookupHandlerClient :: Event -> (Event -> IO Event)
lookupHandlerClient (MouseMove _ _ ) = mouseMoveHandlerClient
lookupHandlerClient (Login _ ) = loginHandlerClient
lookupHandlerClient (Logout) = logoutHandlerClient
lookupHandlerClient (GameNew _ ) = gameNewHandlerClient
lookupHandlerClient (GameJoin _ _ ) = gameJoinHandlerClient
lookupHandlerClient (GameLeave _ _ ) = gameLeaveHandlerClient
lookupHandlerClient (GameStart _ ) = gameStartHandlerClient
lookupHandlerClient (GameAdvance _ ) = gameAdvanceHandlerClient
lookupHandlerClient (GameEnd _ ) = gameEndHandlerClient
lookupHandlerClient (PlayerCollision _ _ ) = playerCollisionHandlerClient
lookupHandlerClient (AlreadyRegistered) = alreadyRegisteredHandlerClient
lookupHandlerClient (UserExists _ ) = userExistsHandlerClient
