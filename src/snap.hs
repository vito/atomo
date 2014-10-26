{-# LANGUAGE QuasiQuotes #-}
import Data.List.Split (wordsBy)
import Snap.Http.Server
import Snap.Types
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell
import Atomo.Pretty


load :: VM ()
load = do
    ([$p|Website|] =::) =<< eval [$e|Object clone do: { routes = [] }|]

    env <- lift get

    [$p|(w: Website) start-on: (port: Integer)|] =: do
        w <- here "w"
        Integer p <- here "port" >>= findValue isInteger

        liftIO $
            httpServe
                (toBS "*")
                (fromIntegral p)
                (toBS "localhost")
                Nothing
                Nothing
                (handle env w)

        return (particle "ok")

handle :: Env -> Value -> Snap ()
handle e w = do
    routes <- liftIO . flip runWith e $ do
        rs <- dispatch (single "routes" w) >>= toList

        forM rs $ \a -> do
            path <- dispatch (single "from" a) >>= toString
            handler <- dispatch (single "to" a)
            return (toBS path, callHandler path e w handler)

    case routes of
        Right rs -> route rs
        Left e -> writeBS (toBS ("500: Internal Server Error\n\n" ++ show (pretty e)))


callHandler :: String -> Env -> Value -> Value -> Snap ()
callHandler p e w h = do
    ps <- fmap rqParams getRequest
    let params = ordered (map tail . filter ((== ':') . head) $ wordsBy (== '/') p) ps

    r <- liftIO $ flip runWith e $ do
        as <- mapM string params
        args <- list (w : as)
        dispatch (keyword ["call"] [h, args])

    case r of
        Right bodyv -> toString bodyv >>= writeBS . toBS
        Left e -> writeBS (toBS ("500: Internal Server Error\n\n" ++ show (pretty e)))
  where
    ordered [] _ = []
    ordered (n:ns) ps =
        case M.lookup (toBS n) ps of
            Just (v:_) -> (fromBS v : ordered ns ps)
            Nothing -> ("" : ordered ns ps)

toBS :: String -> BS.ByteString
toBS = BS.pack . map (fromIntegral . fromEnum)

fromBS :: BS.ByteString -> String
fromBS = map (toEnum . fromIntegral) . BS.unpack
