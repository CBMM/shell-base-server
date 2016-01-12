{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString, append, pack, unpack)
import Data.Maybe
import Snap
import Snap.Http.Server
import Snap.Util.FileServe
import System.Process

cmds :: [ByteString]
cmds = ["doubleit", "cowsay"]

main :: IO ()
main = quickHttpServe
       (serveDirectory "static"
        <|>
        route [("/:cmd/:arg", serveCommand cmds)])

serveCommand :: [ByteString] -> Snap ()
serveCommand okCmds = do
  cmd <- getParam "cmd"
  arg <- maybeToList <$> getParam "arg"
  case cmd of
    Nothing -> writeBS "No command"
    Just c
      | c `notElem` okCmds ->
        writeBS (append c " is not an ok command")
      | otherwise -> do
          retString <- liftIO (readProcess (unpack c) (map unpack arg) "")
          writeBS (pack retString)
