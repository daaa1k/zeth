module Zettelkasten (zet, openFileWithTemplate) where

import System.Environment (getEnv)
import System.IO
import System.Directory
import System.FilePath ((</>), (<.>))
import Data.Time
import Control.Monad (when)
import System.Process (callCommand)

getFilenameFromPrompt :: IO String
getFilenameFromPrompt = do
    putStr "Enter a filename: "
    hFlush stdout
    fmap (filter (/= '\n')) getLine

openFileWithTemplate :: FilePath -> String -> IO () -> IO FilePath
openFileWithTemplate dir filename runEditor = do
    let filepath = dir </> filename <.> "md"
    createDirectoryIfMissing True dir

    fileExists <- doesFileExist filepath
    when (not fileExists) $ writeFile filepath ""

    timestamp <- formatTime defaultTimeLocale "%Y%m%d%H%M" <$> getZonedTime
    appendFile filepath $ "# \n\n\n\nLinks:\n\n" ++ timestamp ++ "\n"

    runEditor

    return filepath

zet :: Maybe String -> IO ()
zet filenameOpt = do
    filename <- maybe getFilenameFromPrompt pure filenameOpt

    if ' ' `elem` filename
      then putStrLn "Please provide a single filename (no spaces, use dashes)."
      else do
        baseDir <- getEnv "ZETTELKASTEN"
        editor  <- getEnv "EDITOR"
        let zettelDir = baseDir </> "0_inbox"
        let runEditor = callCommand (editor ++ " " ++ (zettelDir </> filename <.> "md"))
        _ <- openFileWithTemplate zettelDir filename runEditor
        return ()
