module Paths_hasio_basic (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/magneticduck/.cabal/bin"
libdir     = "/home/magneticduck/.cabal/lib/x86_64-linux-ghc-7.10.2/hasio-basic-0.1.0.0-2MXaTC9hEZg1zfTCXXUUHu"
datadir    = "/home/magneticduck/.cabal/share/x86_64-linux-ghc-7.10.2/hasio-basic-0.1.0.0"
libexecdir = "/home/magneticduck/.cabal/libexec"
sysconfdir = "/home/magneticduck/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hasio_basic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hasio_basic_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hasio_basic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hasio_basic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hasio_basic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
