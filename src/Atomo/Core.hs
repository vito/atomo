module Atomo.Core where

import Control.Concurrent
import Control.Monad.State

import Atomo.Types
import Atomo.Method
import Atomo.Environment


-- | Defines all primitive objects, including the Lobby.
initCore :: VM ()
initCore = do
    -- the very root object
    object <- newObject [] noMethods

    -- top scope is a proto delegating to the root object
    topObj <- newObject [object] noMethods

    modify $ \e -> e { top = topObj }

    -- Lobby is the very bottom scope object
    define (single "Lobby" (PMatch topObj)) (EPrimitive Nothing topObj)

    -- define Object as the root object
    define (single "Object" (PMatch topObj)) (EPrimitive Nothing object)

    -- create parser environment
    parserEnv <- newObject [topObj] noMethods

    modify $ \e -> e
        { primitives = (primitives e) { idObject = object }
        , parserState = (parserState e) { psEnvironment = parserEnv }
        }

    -- this thread's channel
    chan <- liftIO newChan
    modify $ \e -> e { channel = chan }

    -- Numeric object; Integer and Double
    number <- newObject [object] noMethods

    -- define Object as the root object
    define (single "Number" (PMatch topObj)) (EPrimitive Nothing number)

    -- define primitive objects
    forM_ primObjs $ \(n, f) -> do
        o <-
            if n `elem` ["Integer", "Double"]
                then newObject [number] noMethods
                else newObject [object] noMethods

        define (single n (PMatch topObj)) (EPrimitive Nothing o)
        modify $ \e -> e { primitives = f (primitives e) o }
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
        , ("Regexp", \is r -> is { idRegexp = r })
        , ("String", \is r -> is { idString = r })
        , ("Tuple", \is r -> is { idTuple = r })
        ]



