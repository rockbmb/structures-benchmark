module Paths_containers_benchmark (
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
version = Version [1,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rockbmb/.cabal/bin"
libdir     = "/home/rockbmb/.cabal/lib/x86_64-linux-ghc-7.10.3/containers-benchmark-1.1.0.0-B6Y4gzMkL5SARjwgTsQuBd"
datadir    = "/home/rockbmb/.cabal/share/x86_64-linux-ghc-7.10.3/containers-benchmark-1.1.0.0"
libexecdir = "/home/rockbmb/.cabal/libexec"
sysconfdir = "/home/rockbmb/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "containers_benchmark_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "containers_benchmark_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "containers_benchmark_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "containers_benchmark_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "containers_benchmark_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
