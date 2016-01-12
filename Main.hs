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
--
--   NOTE: It's very important that we restrict what commands the user
--         could call! At the point of giving users shell access,
--         the application is only as safe as the shell commands
--         we're calling. We don't want to let the user have access
--         to a compiler, for example: they could write a program that
--         deletes the hard drive or steals other users' data on the
--         server.
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
