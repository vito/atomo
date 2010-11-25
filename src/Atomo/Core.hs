module Atomo.Core where

import Control.Concurrent
import "monads-fd" Control.Monad.State

import Atomo.Types
import Atomo.Environment


-- | Defines all primitive objects, including the Lobby.
initCore :: VM ()
initCore = do
    -- the very root object
    object <- newObject id

    -- top scope is a proto delegating to the root object
    topObj <- newObject $ \o -> o { oDelegates = [object] }
    modify $ \e -> e { top = topObj }

    -- Lobby is the very bottom scope object
    define (single "Lobby" PThis) (Primitive Nothing topObj)

    -- define Object as the root object
    define (single "Object" PThis) (Primitive Nothing object)

    -- create parser environment
    parserEnv <- newObject $ \o -> o { oDelegates = [topObj] }

    modify $ \e -> e
        { primitives = (primitives e) { idObject = rORef object }
        , parserState = (parserState e) { psEnvironment = parserEnv }
        }

    -- this thread's channel
    chan <- liftIO newChan
    modify $ \e -> e { channel = chan }

    -- define primitive objects
    forM_ primObjs $ \(n, f) -> do
        o <- newObject $ \o -> o { oDelegates = [object] }
        define (single n PThis) (Primitive Nothing o)
        modify $ \e -> e { primitives = f (primitives e) (rORef o) }
  where
    primObjs =
        [ ("Block", \is r -> is { idBlock = r })
        , ("Boolean", \is r -> is { idBoolean = r })
        , ("Char", \is r -> is { idChar = r })
        , ("Continuation", \is r -> is { idContinuation = r })
        , ("Double", \is r -> is { idDouble = r })
        , ("Expression", \is r -> is { idExpression = r })
        , ("Haskell", \is r -> is { idHaskell = r })
        , ("Integer", \is r -> is { idInteger = r })
        , ("List", \is r -> is { idList = r })
        , ("Message", \is r -> is { idMessage = r })
        , ("Method", \is r -> is { idMethod = r })
        , ("Particle", \is r -> is { idParticle = r })
        , ("Process", \is r -> is { idProcess = r })
        , ("Pattern", \is r -> is { idPattern = r })
        , ("Rational", \is r -> is { idRational = r })
        , ("String", \is r -> is { idString = r })
        ]



