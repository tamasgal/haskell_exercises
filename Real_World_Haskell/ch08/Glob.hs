module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)


isPattern :: String -> Bool
isPattern = any (`elem` "[*?")


namesMatching pat
    | not (isPattern pat) = do
      exists <- doesNameExist pat
      return [pat | exists]
    | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
            curDir <- getCurrentDirectory
            listMatches curDir baseName
        (dirName, BaseName) -> do
            dirs <- if isPattern dirName
                    then namesMatching (dropTrailingPathSeparator dirName)
                    else return [dirName]
            let listDir = if isPattern baseName
                          then listMatches
                          else listPlain
            pathNames <- forM dirs $ \dir -> do
                            baseNames <- listDir dir baseName
                            return (map (dir </>) baseNames)
            return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExists name
    if fileExists
        then return True
        else doesDirectoryExists name
