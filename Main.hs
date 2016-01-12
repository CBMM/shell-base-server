{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative    ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8  (ByteString, pack, unpack)
import Data.Maybe             (maybeToList)
import Data.Monoid            ((<>), mempty)
import Snap                   (Snap, getParam, route, writeBS)
import Snap.Http.Server       (commandLineConfig, httpServe)
import Snap.Util.FileServe    (serveDirectory)
import System.Process         (readProcess)

------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
-- | Main function chooses between serving static assets
--   and handling command requests
main :: IO ()
main = do
  cfg <- commandLineConfig mempty
  httpServe cfg
       (serveDirectory "static"
        <|>
        route [("/:cmd/:arg", serveCommand cmds)])

------------------------------------------------------------------------------
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
        writeBS (c <> " is not an ok command")
      | otherwise -> do
          retString <- liftIO (readProcess (unpack c) (map unpack arg) "")
          writeBS (pack retString)
