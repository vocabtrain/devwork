{-# LANGUAGE TemplateHaskell #-}

module Version where

import Language.Haskell.TH
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Process


getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

version = do
    a <- runIO $ getRecursiveContents "."
    listE $ map stringE a                                      


-- then: $(version) lists all files in compile-time!
--
--
-- gravatar:
-- import Data.Digest.Pure.MD5 (md5)
-- import Data.Text            (Text)
-- "http://www.gravatar.com/avatar/"
--         md5sum :: Text -> String
--                 md5sum = show . md
--                 import Data.Digest.Pure.MD5 (md5)
--                 import Data.Text            (Text)5 . C8.pack . T.unpack . T.toLower . T.strip
