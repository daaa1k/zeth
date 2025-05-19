module Main where

import Test.HUnit
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Zettelkasten

testOpenFileCreatesFile :: Test
testOpenFileCreatesFile = TestCase $
    withSystemTempDirectory "zettel-test" $ \dir -> do
        let filename = "test-note"
        path <- openFileWithTemplate dir filename (return ())
        exists <- doesFileExist path
        assertBool "File should exist" exists
        content <- readFile path
        assertBool "Content should include Links:" ("Links:" `elem` lines content)

main :: IO ()
main = runTestTTAndExit $ TestList
    [ TestLabel "openFileWithTemplate creates file" testOpenFileCreatesFile ]
