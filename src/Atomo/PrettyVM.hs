module Atomo.PrettyVM where

import "monads-fd" Control.Monad.State
import qualified Text.PrettyPrint as P

import Atomo.Environment
import Atomo.Pretty
import Atomo.Types


-- | print an error, including the previous 10 expressions evaluated
-- with the most recent on the bottom
printError :: AtomoError -> VM ()
printError err = do
    t <- traceback

    if not (null t)
        then do
            liftIO (putStrLn "traceback:")

            forM_ t $ \e -> liftIO $
                print (prettyStack e)

            liftIO (putStrLn "")
        else return ()

    prettyError err >>= liftIO . print

    modify $ \s -> s { stack = [] }
  where
    traceback = liftM (reverse . take 10 . reverse) (gets stack)

prettyError :: AtomoError -> VM P.Doc
prettyError (Error v) = liftM (P.text "error:" P.<+>) (prettyVM v)
prettyError e = return (pretty e)

-- | pretty-print by sending \@show to the object
prettyVM :: Value -> VM P.Doc
prettyVM = liftM (P.text . fromText . fromString) . dispatch . (single "show")

