module Main where

import Control.Lens
-- import Control.Lens.Plated
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Data.Lens
import Data.Foldable
import qualified Codec.Archive.Zip as Zip
import System.Environment
import qualified Data.Text as T

import DA.Daml.LF.Ast.Base
import DA.Daml.LF.Ast.Optics (moduleExpr)
import DA.Daml.LF.Ast.Util
import qualified DA.Daml.LF.Proto3.Archive as Archive
import DA.Daml.LF.Reader

main :: IO ()
main = do
    [file] <- getArgs
    darBytes <- BS.readFile file
    Right (Dalfs {mainDalf}) <- pure $ readDalfs $ Zip.toArchive (BSL.fromStrict darBytes)
    Right (_, pkg) <- pure $ Archive.decodeArchive Archive.DecodeAsMain $ BSL.toStrict mainDalf
    for_ (packageModules pkg) $ \mod ->
      let go :: Expr -> IO ()
          go (ETyApp (EVal (Qualified _  (ModuleName ["DA", "Internal", "Template"]) (ExprValName "create"))) t) =
              case t of
                  TCon _ -> pure ()
                  ty -> putStrLn $ modName <> ": polymorphic create: " <> show ty
          go (ETyApp (ETyApp (EVal (Qualified _ (ModuleName ["DA", "Internal", "Template"]) (ExprValName "exercise"))) tplTy) choiceTy) =
              case (tplTy, choiceTy) of
                  (TCon _, TCon _) -> pure ()
                  _ -> putStrLn $ modName <> ": polymorphic exercise: " <> show (tplTy, choiceTy)
          go (ETyApp (EVal (Qualified _ (ModuleName ["DA", "Internal", "Template"]) (ExprValName "fetch"))) tplTy) =
              case tplTy of
                  TCon _ -> pure ()
                  _ -> putStrLn $ modName <> ": polymorphic fetch: " <> show tplTy
          go (ETyApp (EVal (Qualified _ (ModuleName ["DA", "Internal", "Template"]) (ExprValName "archive"))) tplTy) =
              case tplTy of
                  TCon _ -> pure ()
                  _ -> putStrLn $ modName <> ": polymorphic archive: " <> show tplTy
          go (ETyApp (EVal (Qualified _ (ModuleName ["DA", "Internal", "Template"]) (ExprValName "signatory"))) tplTy) =
              case tplTy of
                  TCon _ -> pure ()
                  _ -> putStrLn $ modName <> ": polymorphic signatory: " <> show tplTy
          go (ETyApp (EVal (Qualified _ (ModuleName ["DA", "Internal", "Template"]) (ExprValName "observer"))) tplTy) =
              case tplTy of
                  TCon _ -> pure ()
                  _ -> putStrLn $ modName <> ": polymorphic observer: " <> show tplTy
          go (EVal (Qualified _  (ModuleName ["DA", "Internal", "Template"]) (ExprValName "create"))) = error "should never happen"
          go (EVal (Qualified _  (ModuleName ["DA", "Internal", "Template"]) (ExprValName "exercise"))) = error "should never happen"
          go (EVal (Qualified _  (ModuleName ["DA", "Internal", "Template"]) (ExprValName "fetch"))) = error "should never happen"
          go (EVal (Qualified _  (ModuleName ["DA", "Internal", "Template"]) (ExprValName "archive"))) = error "should never happen"
          go (EVal (Qualified _  (ModuleName ["DA", "Internal", "Template"]) (ExprValName "signatory"))) = error "should never happen"
          go (EVal (Qualified _  (ModuleName ["DA", "Internal", "Template"]) (ExprValName "observer"))) = error "should never happen"
          go e = forOf_ uniplate e go
          modName = T.unpack $ moduleNameString $ moduleName mod
       in forOf_ moduleExpr mod $ go
