{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_exercises (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/arielle/Documents/adel0037/Haskell/.stack-work/install/aarch64-osx/ff799dca821a5c5e69a316b4d936a7ea85db710bde888f85f07ca76d0f3d68ef/9.2.8/bin"
libdir     = "/Users/arielle/Documents/adel0037/Haskell/.stack-work/install/aarch64-osx/ff799dca821a5c5e69a316b4d936a7ea85db710bde888f85f07ca76d0f3d68ef/9.2.8/lib/aarch64-osx-ghc-9.2.8/exercises-0.1.0.0-KKCc0oKYCwX9Gf3CkVmKmT-main"
dynlibdir  = "/Users/arielle/Documents/adel0037/Haskell/.stack-work/install/aarch64-osx/ff799dca821a5c5e69a316b4d936a7ea85db710bde888f85f07ca76d0f3d68ef/9.2.8/lib/aarch64-osx-ghc-9.2.8"
datadir    = "/Users/arielle/Documents/adel0037/Haskell/.stack-work/install/aarch64-osx/ff799dca821a5c5e69a316b4d936a7ea85db710bde888f85f07ca76d0f3d68ef/9.2.8/share/aarch64-osx-ghc-9.2.8/exercises-0.1.0.0"
libexecdir = "/Users/arielle/Documents/adel0037/Haskell/.stack-work/install/aarch64-osx/ff799dca821a5c5e69a316b4d936a7ea85db710bde888f85f07ca76d0f3d68ef/9.2.8/libexec/aarch64-osx-ghc-9.2.8/exercises-0.1.0.0"
sysconfdir = "/Users/arielle/Documents/adel0037/Haskell/.stack-work/install/aarch64-osx/ff799dca821a5c5e69a316b4d936a7ea85db710bde888f85f07ca76d0f3d68ef/9.2.8/etc"

getBinDir     = catchIO (getEnv "exercises_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "exercises_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "exercises_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "exercises_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercises_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
