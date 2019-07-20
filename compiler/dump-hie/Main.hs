module Main where

import Control.Monad.IO.Class

import HieBin
import HieDebug
import HieTypes
import NameCache
import UniqSupply
import Outputable

import System.Environment

import DA.Daml.Options

main :: IO ()
main = do
  [hieFilePath] <- getArgs
  u <- mkSplitUniqSupply 'a'
  let nameCache = initNameCache u []
  (HieFileResult _ _ hieFile, _) <- readHieFile nameCache hieFilePath
  runGhcFast $ do
    liftIO $ putStrLn $ showSDocUnsafe $ ppHies $ hie_asts hieFile
