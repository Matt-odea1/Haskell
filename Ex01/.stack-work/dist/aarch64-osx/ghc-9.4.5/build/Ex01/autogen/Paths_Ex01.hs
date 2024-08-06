{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Ex01 (
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
version = Version [1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/matthewodea/Desktop/haskell/Ex01/.stack-work/install/aarch64-osx/5eb0f641b1f5051ac9e65a32bee152d77eeafcb4cd32d369b610d476a060f1a1/9.4.5/bin"
libdir     = "/Users/matthewodea/Desktop/haskell/Ex01/.stack-work/install/aarch64-osx/5eb0f641b1f5051ac9e65a32bee152d77eeafcb4cd32d369b610d476a060f1a1/9.4.5/lib/aarch64-osx-ghc-9.4.5/Ex01-1.0-3Khqvy7sl3f5H09z4ZpSDc-Ex01"
dynlibdir  = "/Users/matthewodea/Desktop/haskell/Ex01/.stack-work/install/aarch64-osx/5eb0f641b1f5051ac9e65a32bee152d77eeafcb4cd32d369b610d476a060f1a1/9.4.5/lib/aarch64-osx-ghc-9.4.5"
datadir    = "/Users/matthewodea/Desktop/haskell/Ex01/.stack-work/install/aarch64-osx/5eb0f641b1f5051ac9e65a32bee152d77eeafcb4cd32d369b610d476a060f1a1/9.4.5/share/aarch64-osx-ghc-9.4.5/Ex01-1.0"
libexecdir = "/Users/matthewodea/Desktop/haskell/Ex01/.stack-work/install/aarch64-osx/5eb0f641b1f5051ac9e65a32bee152d77eeafcb4cd32d369b610d476a060f1a1/9.4.5/libexec/aarch64-osx-ghc-9.4.5/Ex01-1.0"
sysconfdir = "/Users/matthewodea/Desktop/haskell/Ex01/.stack-work/install/aarch64-osx/5eb0f641b1f5051ac9e65a32bee152d77eeafcb4cd32d369b610d476a060f1a1/9.4.5/etc"

getBinDir     = catchIO (getEnv "Ex01_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Ex01_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Ex01_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Ex01_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ex01_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ex01_sysconfdir") (\_ -> return sysconfdir)




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
