{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString, append, pack, unpack)
import Data.Maybe
import Snap
import Snap.Http.Server
import Snap.Util.FileServe
import System.Process

-- | List of shell commands available to the server
--   When a user tries to execute a command, the server
--   will make sure the command is in this list
cmds :: [ByteString]
cmds = ["doubleit", "cowsay"]

-- | Main function chooses between serving static assets
--   and handling command requests
main :: IO ()
main = quickHttpServe
       (serveDirectory "static"
        <|>
        route [("/:cmd/:arg", serveCommand cmds)])

-- | This handler extracts the command name and argument string
--   from the request, runs it, and responds with the command's output
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
