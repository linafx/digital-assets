{-# OPTIONS_GHC -Wwarn #-}
module Main where

import qualified Data.ByteString as BS
import qualified DA.Daml.LF.Proto3.Decode as Decode
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Proto3.Archive as Archive
import qualified Proto3.Suite as Proto
import System.Time.Extra
import Control.Exception
import System.Environment

main :: IO ()
main = do
    -- print ">>="
    [file] <- getArgs
    bytes <- BS.readFile file
    (time, bs) <- duration $ do
        Right (pkgId, bs) <- pure $ Archive.decodeArchivePayload bytes
        _ <- evaluate pkgId
        _ <- evaluate bs
        pure bs
    putStrLn $ ("decodeArchivePayload: " <> showDuration time)
    (time, payload) <- duration $ do
        Right payload <- pure $ Proto.fromByteString bs
        _ <- evaluate payload
        pure payload
    putStrLn $ ("payload read: " <> showDuration time)
    (time, _package) <- duration $ do
        Right package <- pure $ Decode.decodePayload LF.PRSelf payload
        _ <- evaluate package
        pure package
    putStrLn $ ("decodePayload: " <> showDuration time)
