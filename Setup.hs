import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

import System.Directory
import System.Exit
import System.FilePath

main = defaultMainWithHooks simpleUserHooks
    { postInst = installEco
    }

installEco :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installEco _ _ _ _ = do
    home <- getHomeDirectory

    mapM_ (createDirectoryIfMissing True . (ecosystem home </>))
        ["bin", "lib"]

    copyFile eco (toEco home)

    from <- getPermissions (toEco home)
    setPermissions (toEco home) (from { executable = True })
  where
    ecosystem h = h </> ".eco"

    eco = "bin" </> "eco"
    toEco h = ecosystem h </> eco
