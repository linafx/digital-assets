-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pretty-printing of scenario results
module DA.Daml.LF.PrettyScenario
  ( prettyScenarioResult
  , prettyScenarioError
  , prettyBriefScenarioError
  , renderScenarioResult
  , lookupDefLocation
  , ModuleRef
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           DA.Daml.LF.Decimal  (stringToDecimal)
import qualified DA.Daml.LF.Ast             as LF
import Control.Applicative
import Text.Read hiding (parens)
import           DA.Pretty as Pretty
import           Data.Either.Extra
import           Data.Int
import Data.List
import Data.Maybe
import qualified Data.NameMap               as NM
import qualified Data.Map.Strict            as MS
import Data.ProtoLens.Labels ()
import qualified Data.Ratio                 as Ratio
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Extended         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Time.Clock.POSIX      as CP
import qualified Data.Time.Format           as TF
import qualified Data.Vector                as V
import qualified Network.URI.Encode
import Proto.Compiler.ScenarioService.Protos.ScenarioService as Proto
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as Blaze

data Error = ErrorMissingNode NodeId
type M = ExceptT Error (Reader (MS.Map NodeId Node, LF.World))

type ModuleRef = LF.Qualified ()

runM :: [Proto.Node] -> LF.World -> M (Doc SyntaxClass) -> Doc SyntaxClass
runM nodes world =
  ppError . flip runReader (nodeMap, world) . runExceptT
  where
    nodeMap = MS.fromList
        [ (nodeId, node)
        | node <- nodes
        , let nodeId = node ^. #nodeId
        ]
    ppError :: Either Error (Doc SyntaxClass) -> Doc SyntaxClass
    ppError (Right x) = x
    ppError (Left (ErrorMissingNode nodeId)) =
      angledList [ text "missing node",  prettyNodeId nodeId ]

lookupNode :: NodeId -> M Node
lookupNode nodeId = asks (MS.lookup nodeId . fst) >>= \case
  Nothing -> throwE (ErrorMissingNode nodeId)
  Just node -> pure node

askWorld :: M LF.World
askWorld = asks snd

lookupDefLocation :: LF.Module -> T.Text -> Maybe LF.SourceLoc
lookupDefLocation mod0 defName =
  join $
    LF.dvalLocation <$> NM.lookup (LF.ExprValName defName) (LF.moduleValues mod0)
    <|>
    LF.tplLocation <$> NM.lookup (LF.TypeConName [defName]) (LF.moduleTemplates mod0)

lookupModule :: LF.World -> Maybe Proto.PackageIdentifier -> LF.ModuleName -> Maybe LF.Module
lookupModule world mbPkgId modName = do
    let pkgRef = case mbPkgId ^? _Just . #maybe'packageId . _Just of
            Just pkgId -> LF.PRImport $ LF.PackageId $ pkgId
            Nothing -> LF.PRSelf
    eitherToMaybe (LF.lookupModule (LF.Qualified pkgRef modName ()) world)

lookupModuleFromQualifiedName ::
     LF.World -> Maybe Proto.PackageIdentifier -> T.Text
  -> Maybe (LF.ModuleName, T.Text, LF.Module)
lookupModuleFromQualifiedName world mbPkgId qualName = do
  let (modName, defName) = case T.splitOn ":" qualName of
        [modNm, defNm] -> (LF.ModuleName (T.splitOn "." modNm), defNm)
        _ -> error "Bad definition"
  modu <- lookupModule world mbPkgId modName
  return (modName, defName, modu)

parseNodeId :: Proto.NodeId -> [Integer]
parseNodeId =
    fmap (fromMaybe 0 . readMaybe . T.unpack)
  . T.splitOn ":"
  . view #id

-- prettyScenarioResult
--   :: LF.World -> Proto.ScenarioResult -> Doc SyntaxClass
-- prettyScenarioResult world scenarioResult =
--   let ppSteps = runM (scenarioResult ^. #nodes) world (vsep <$> mapM prettyScenarioStep (V.toList steps))
--       isActive node = case node ^? #maybe'create of
--         Just _ -> isNothing $ node ^? #consumedBy
--         _ -> False
--       sortNodeIds = sortBy (\a b -> compare (parseNodeId a) (parseNodeId b))
--       ppActive =
--           fcommasep
--         $ map prettyNodeIdLink
--         $ sortNodeIds $ mapMaybe nodeNodeId
--         $ filter isActive (V.toList nodes)

--       ppTrace = vcat $ map (prettyTraceMessage world) (V.toList traceLog)
--   in vsep
--     [ label_ "Transactions: " ppSteps
--     , label_ "Active contracts: " ppActive
--     , maybe mempty (\v -> label_ "Return value:" (prettyValue' True 0 world v)) retValue
--     , if V.null traceLog
--       then text ""
--       else text "Trace: " $$ nest 2 ppTrace
--     ]

prettyBriefScenarioError
  :: LF.World -> ScenarioError -> Doc SyntaxClass
prettyBriefScenarioError world scenarioError = runM (scenarioError ^. #nodes) world $ do
  ppError <- prettyScenarioErrorError (scenarioError ^. #maybe'error)
  pure $
    annotateSC ErrorSC
      (text "Scenario execution" <->
        (if isNothing (scenarioError ^. #maybe'commitLoc)
         then "failed:"
         else "failed on commit at"
           <-> prettyMayLocation world (scenarioError ^. #maybe'commitLoc) <> char ':')
      )
    $$ nest 2 ppError

prettyScenarioError
  :: LF.World -> ScenarioError -> Doc SyntaxClass
prettyScenarioError world scenarioError = runM (scenarioError ^. #nodes) world $ do
  ppError <- prettyScenarioErrorError (scenarioError ^. #maybe'error)
  ppSteps <- vsep <$> mapM prettyScenarioStep (scenarioError ^. #scenarioSteps)
  ppPtx <- forM (scenarioError ^. #maybe'partialTransaction) $ \ptx -> do
      p <- prettyPartialTransaction ptx
      pure $ text "Partial transaction:" $$ nest 2 p
  let ppTrace =
        vcat
          (map (prettyTraceMessage world)
               (scenarioError ^. #traceLog))
  pure $
    vsep $ catMaybes
    [ Just $ error_ (text "Scenario execution" <->
      (if isNothing (scenarioError ^. #maybe'commitLoc)
       then "failed:"
       else "failed on commit at"
         <-> prettyMayLocation world (scenarioError ^. #maybe'commitLoc) <> char ':'))
      $$ nest 2 ppError

    , if isNothing (scenarioError ^. #maybe'lastLoc)
      then Nothing
      else Just $ label_ "Last source location:"
                $ prettyMayLocation world (scenarioError ^. #maybe'lastLoc)

    , Just $ "Ledger time:" <-> prettyTimestamp (scenarioError ^. #ledgerTime)

    , ppPtx

    , if null (scenarioError ^. #scenarioSteps)
      then Nothing
      else Just $ text "Committed transactions: " $$ nest 2 ppSteps

    , if null (scenarioError ^. #traceLog)
      then Nothing
      else Just $ text "Trace: " $$ nest 2 ppTrace
    ]

prettyTraceMessage
  :: LF.World -> TraceMessage -> Doc SyntaxClass
prettyTraceMessage _world msg =
  -- TODO(JM): Locations are still missing in DAML 1.2, and
  -- that's the only place where we get traces.
  --  prettyMayLocation world (traceMessageLocation msg)
  text (msg ^. #message)

prettyScenarioErrorError :: Maybe ScenarioError'Error -> M (Doc SyntaxClass)
prettyScenarioErrorError Nothing = pure $ text "<missing error details>"
prettyScenarioErrorError (Just err) =  do
  world <- askWorld
  case err of
    ScenarioError'Crash reason -> pure $ text "CRASH:" <-> text reason
    ScenarioError'UserError reason -> pure $ text "Aborted: " <-> text reason
    ScenarioError'TemplatePrecondViolated err -> do
      pure $
        "Template pre-condition violated in:"
          $$ nest 2
          (   "create"
          <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          $$ (   keyword_ "with"
              $$ nest 2 (prettyMay "<missing argument>" (prettyValue' False 0 world) (err ^. #maybe'arg))
             )
          )
    ScenarioError'UpdateLocalContractNotActive err ->
      pure $ vcat
        [ "Attempt to exercise a contract that was consumed in same transaction."
        , "Contract:"
            <-> prettyMay "<missing contract>"
                  (prettyContractRef world)
                  (err ^. #maybe'contractRef)
        ]
    ScenarioError'ScenarioContractNotActive err ->
      pure $ vcat
        [ "Attempt to exercise a consumed contract"
            <-> prettyMay "<missing contract>"
                  (prettyContractRef world)
                  (err ^. #maybe'contractRef)
        , "Consumed by:"
            <-> prettyMay "<missing node id>" prettyNodeIdLink (err ^. #maybe'consumedBy)
        ]
    ScenarioError'ScenarioCommitError err -> case err ^. #maybe'sum of
      Nothing -> pure "Unknown commit error"

      Just (CommitError'FailedAuthorizations failedAuth) -> pure $ vcat $ map (prettyFailedAuthorization world) (failedAuth ^. #failedAuthorizations)

      Just (CommitError'UniqueKeyViolation uniqueKeyViolation) -> pure $ vcat
        [ "Commit error due to unique key violation for key"
        , nest 2 (prettyMay "<missing key>" (prettyValue' False 0 world) (uniqueKeyViolation ^.  #maybe'key))
        , "for template"
        , nest 2 (prettyMay "<missing template id>" (prettyDefName world) (uniqueKeyViolation ^. #maybe'templateId))
        ]

    ScenarioError'UnknownContext ctxId ->
      pure $ "Unknown scenario interpretation context:" <-> string (show ctxId)
    ScenarioError'UnknownScenario name ->
      pure $ "Unknown scenario:" <-> prettyDefName world name
    ScenarioError'ScenarioContractNotEffective err ->
      pure $ vcat
        [ "Attempt to fetch or exercise a contract not yet effective."
        , "Contract:"
        <-> prettyMay "<missing contract>" (prettyContractRef world)
              (err ^. #maybe'contractRef)
        , "Effective at:"
        <-> prettyTimestamp (err ^. #effectiveAt)
        ]

    ScenarioError'ScenarioMustfailSucceeded _ ->
      pure "A must-fail commit succeeded."
    ScenarioError'ScenarioInvalidPartyName name ->
      pure $ "Invalid party name: " <-> text name
    ScenarioError'ScenarioContractNotVisible err ->
      pure $ vcat
        [ "Attempt to fetch or exercise a contract not visible to the committer."
        , label_ "Contract: "
            $ prettyMay "<missing contract>"
                (prettyContractRef world)
                (err ^. #maybe'contractRef)
        , label_ "Committer:" $ prettyMay "<missing party>" prettyParty (err ^. #maybe'committer)
        , label_ "Disclosed to:"
            $ fcommasep
            $ map prettyParty (err ^. #observers) -- scenarioError_ContractNotVisibleObservers
        ]
    ScenarioError'SubmitterNotInMaintainers' err ->
      pure $ vcat
        [ "When looking up or fetching a contract of type" <->
            prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId) <-> "by key, submitter:" <->
            prettyMay "<missing submitter>" prettyParty (err ^. #maybe'submitter)
        , "is not in maintainers:" <-> fcommasep (map prettyParty (err ^. #maintainers))
        ]

prettyFailedAuthorization :: LF.World -> FailedAuthorization -> Doc SyntaxClass
prettyFailedAuthorization world failedAuth =
  hcat
    [ prettyMay "<missing node id>" prettyNodeIdLink (failedAuth ^. #maybe'nodeId)
    , text ": "
    , case failedAuth ^. #maybe'sum of
        Just (FailedAuthorization'CreateMissingAuthorization' err) ->
          "create of" <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
          $$
            ("failed due to a missing authorization from"
             <->
             ( fcommasep
             $ map prettyParty
             $ S.toList
             $ S.fromList (err ^. #requiredAuthorizers)
               `S.difference`
               S.fromList (err ^. #authorizingParties)
             )
            )

        Just (FailedAuthorization'MaintainersNotSubsetOfSignatories' err) ->
          "create of" <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
          $$
            ("failed due to that some parties are maintainers but not signatories: "
             <->
             ( fcommasep
             $ map prettyParty
             $ S.toList
             $ S.fromList (err ^. #maintainers)
               `S.difference`
               S.fromList (err ^. #signatories)
             )
            )

        Just (FailedAuthorization'FetchMissingAuthorization' err) ->
          "fetch of" <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
          $$
            ("failed since none of the stakeholders"
             <->
             (fcommasep $ map prettyParty $ err ^. #stakeholders))
          $$
            ("is in the authorizing set"
             <->
             (fcommasep $ map prettyParty $ err ^. #authorizingParties))

        Just (FailedAuthorization'ExerciseMissingAuthorization' err) ->
          "exercise of" <-> prettyChoiceId world (err ^. #maybe'templateId) (err ^. #choiceId)
          <-> "in" <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
          $$
            ("failed due to a missing authorization from"
             <->
             ( fcommasep
             $ map prettyParty
             $ S.toList
             $ S.fromList (err ^. #requiredAuthorizers)
               `S.difference`
               S.fromList (err ^. #authorizingParties)
             )
            )

        Just (FailedAuthorization'ActorMismatch' err) ->
          "exercise of" <-> prettyChoiceId world (err ^. #maybe'templateId) (err ^. #choiceId)
          <-> "in" <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
          $$
            ("failed due to authorization error:"
            $$ "the choice's controlling parties"
            <-> brackets (fcommasep (map prettyParty (err ^. #controllers)))
            $$ "is not a subset of the authorizing parties"
            <-> brackets (fcommasep (map prettyParty (err ^. #givenActors)))
            )

        Just (FailedAuthorization'NoControllers' err) ->
          "exercise of" <-> prettyChoiceId world (err ^. #maybe'templateId) (err ^. #choiceId)
          <-> "in" <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
          $$ "failed due missing controllers"

        Just (FailedAuthorization'NoSignatories' err) ->
          "create of"
          <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
          <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
          $$ "failed due missing signatories"

        Just (FailedAuthorization'LookupByKeyMissingAuthorization' err) ->
         "lookup by key of" <-> prettyMay "<missing template id>" (prettyDefName world) (err ^. #maybe'templateId)
         <-> "at" <-> prettyMayLocation world (err ^. #maybe'location)
         $$
            ("failed due to a missing authorization from"
             <->
             ( fcommasep
             $ map prettyParty
             $ S.toList
             $ S.fromList (err ^. #maintainers)
               `S.difference`
               S.fromList (err ^. #authorizingParties)
             )
            )

        Nothing ->
          text "<missing failed_authorization>"
    ]


prettyScenarioStep :: ScenarioStep -> M (Doc SyntaxClass)
prettyScenarioStep step =
    case step ^. #maybe'step of
        Nothing -> pure $ text "<missing scenario step>"
        Just step' ->  do
            world <- askWorld
            case step' of
              ScenarioStep'Commit' commit
                | Just tx <- commit ^. #maybe'tx ->
                prettyCommit (commit ^. #txId) (commit ^. #maybe'location) tx

              ScenarioStep'AssertMustFail' assertMustFail ->
                pure
                    $ idSC ("n" <> TE.show (assertMustFail ^. #txId)) (keyword_ "TX")
                  <-> prettyTxId (assertMustFail ^. #txId)
                  <-> prettyTimestamp (assertMustFail ^. #time)
                   $$ (nest 3
                       $   keyword_ "mustFailAt"
                       <-> prettyParty (assertMustFail ^. #actor)
                       <-> parens (prettyMayLocation world (assertMustFail ^. #maybe'location))
                      )

              ScenarioStep'PassTime dtMicros ->
                pure
                    $ idSC ("n" <> TE.show (step ^. #stepId)) (keyword_ "pass")
                    <-> prettyTxId (step ^. #stepId)
                    <-> text (relTimeToText dtMicros)

              bad ->
                pure $ text "INVALID STEP:" <-> string (show bad)
            where
              relTimeToText :: Int64 -> T.Text
              relTimeToText micros
                | micros == 0 = "0s"
                | n <= 6 = fixup $ '0' : '.' : trim (replicate (6 - n) '0' ++ dt)
                | otherwise = fixup $
                    let (prefix, trim -> suffix) = splitAt (n - 6) dt
                    in if null suffix then prefix else prefix ++ '.' : suffix
                where
                  trim = dropWhileEnd ('0' ==)
                  fixup cs
                    | micros < 0 = T.pack ('-' : cs) <> "s"
                    | otherwise = T.pack cs <> "s"
                  dt = show (abs micros)
                  n  = length dt

prettyTimestamp :: Int64 -> Doc SyntaxClass
prettyTimestamp = prettyUtcTime . toUtcTime . fromIntegral
  where
    prettyUtcTime =
      string . TF.formatTime TF.defaultTimeLocale "%FT%T%QZ"
    toUtcTime t =
        CP.posixSecondsToUTCTime
      $ fromRational $ t Ratio.% (10 ^ (6 :: Integer))

prettyCommit :: Int32 -> Maybe Location -> Transaction -> M (Doc SyntaxClass)
prettyCommit txid mbLoc trans = do
  world <- askWorld
  children <- vsep <$> mapM (lookupNode >=> prettyNode) (trans ^. #roots)
  return
      $ idSC ("n" <> TE.show txid) (keyword_ "TX")
    <-> prettyTxId txid
    <-> prettyTimestamp (trans ^. #effectiveAt)
    <-> parens (prettyMayLocation world mbLoc)
     $$ children

prettyMayLocation :: LF.World -> Maybe Location -> Doc SyntaxClass
prettyMayLocation _ Nothing = text "unknown source"
prettyMayLocation world (Just loc) =
      maybe id (\path -> linkSC (url path) title)
        (lookupModule world (loc ^. #maybe'package) (LF.ModuleName (T.splitOn "." (loc ^. #module'))) >>= LF.moduleSource)
    $ text title
  where
    encodeURI = Network.URI.Encode.encodeText
    title = (loc ^. #module') <> lineNum
    url fp = "command:daml.revealLocation?"
      <> encodeURI ("[\"file://" <> T.pack fp <> "\", "
      <> TE.show (loc ^. #startLine) <> ", " <> TE.show (loc ^. #endLine) <> "]")

    lineNum = ":" <> pint32 (loc ^. #startLine) <> ":" <> pint32 (loc ^. #startCol)
    pint32 :: Int32 -> T.Text
    pint32 = TE.show . (1+) -- locations are 0-indexed.



prettyTxId :: Int32 -> Doc SyntaxClass
prettyTxId txid =
  linkToIdSC ("n" <> TE.show txid) $ char '#' <> string (show txid)

linkToIdSC :: T.Text -> Doc SyntaxClass -> Doc SyntaxClass
linkToIdSC targetId =
  -- This seems to be the only way to easily scroll to a anchor within
  -- the iframe inside VSCode. One downside is that it's a no-op on further
  -- clicks.
    annotateSC ConstructorSC
  . annotateSC (OnClickSC $ "window.location.hash='" <> targetId <> "';")

prettyNodeId :: Proto.NodeId -> Doc SyntaxClass
prettyNodeId nodeId =
    idSC ("n" <> nodeId')
  $ annotateSC ConstructorSC
  $ char '#' <> text nodeId'
  where nodeId' = nodeId ^. #id

prettyNodeIdLink :: NodeId -> Doc SyntaxClass
prettyNodeIdLink nodeId =
  linkToIdSC ("n" <> nodeId ^. #id) $ char '#' <> text (nodeId ^. #id)

prettyContractId :: T.Text -> Doc SyntaxClass
prettyContractId coid =
  linkToIdSC ("n" <> coid) $ char '#' <> text coid

linkSC :: T.Text -> T.Text -> Doc SyntaxClass -> Doc SyntaxClass
linkSC url title = annotateSC (LinkSC url title)

idSC :: T.Text -> Doc SyntaxClass -> Doc SyntaxClass
idSC id_ = annotateSC (IdSC id_)

prettyMay :: T.Text -> (x -> Doc SyntaxClass) -> Maybe x -> Doc SyntaxClass
prettyMay miss _ Nothing = text miss
prettyMay _ p (Just x)   = p x

prettyMayParty :: Maybe Party -> Doc SyntaxClass
prettyMayParty Nothing  = text "<missing party>"
prettyMayParty (Just p) = prettyParty p

prettyParty :: Party -> Doc SyntaxClass
prettyParty p = text ("'" <> p ^. #party <> "'")

prettyNodeNode :: Node'Node -> M (Doc SyntaxClass)
prettyNodeNode nn = do
  world <- askWorld
  case nn of
    Node'Create' create ->
      pure $
        case create ^. #maybe'contractInstance of
          Nothing -> text "<missing contract instance>"
          Just inst ->
             (keyword_ "create" <-> prettyMay "<TEMPLATE?>" (prettyDefName world) (inst ^. #maybe'templateId))
           $$ maybe
                mempty
                (\v ->
                  keyword_ "with" $$
                    nest 2 (prettyValue' False 0 world v))
                  (inst ^. #maybe'value)

    Node'Fetch' fetch ->
      pure $
        keyword_ "fetch"
          <-> prettyContractId (fetch ^. #contractId)
          <-> maybe mempty
                  (\tid -> parens (prettyDefName world tid))
                  (fetch ^. #maybe'templateId)

    Node'Exercise' exercise -> do
      ppChildren <-
        if null (exercise ^. #children)
        then pure mempty
        else
              (keyword_ "children:" $$) . vsep
          <$> mapM (lookupNode >=> prettyNode) (exercise ^. #children)

      pure
        $   fcommasep (map prettyParty $ exercise ^. #actingParties)
        <-> ( -- group to align "exercises" and "with"
              keyword_ "exercises"
          <-> hsep
              [ prettyChoiceId world (exercise ^. #maybe'templateId) (exercise ^. #choiceId)
              , keyword_ "on"
              , prettyContractId (exercise ^. #targetContractId)
              , parens (prettyMay "<missing TemplateId>" (prettyDefName world) $ exercise ^. #maybe'templateId)
              ]
           $$ if isUnitValue (exercise ^. #maybe'chosenValue)
              then mempty
              else keyword_ "with"
                $$ nest 2
                     (prettyMay "<missing value>"
                       (prettyValue' False 0 world) $
                       exercise ^. #maybe'chosenValue)
        )
        $$ ppChildren

    Node'LookupByKey' lookupByKey -> do
      pure $
        keyword_ "lookupByKey"
          <-> prettyMay "<TEMPLATE?>" (prettyDefName world) (lookupByKey ^. #maybe'templateId)
          $$ text "with key"
          $$ nest 2
            (prettyMay "<KEY?>"
              (prettyMay "<KEY?>" (prettyValue' False 0 world) . view #maybe'key) $
              lookupByKey ^. #maybe'keyWithMaintainers)
          $$ if T.null (lookupByKey ^. #contractId)
            then text "not found"
            else text "found:" <-> text (lookupByKey ^. #contractId)

isUnitValue :: Maybe Value -> Bool
isUnitValue v
    | Just _ <- v ^? _Just . #maybe'unit . _Just = True
    | otherwise = False

prettyNode :: Node -> M (Doc SyntaxClass)
prettyNode n
  | Nothing <- n ^. #maybe'node =
      pure "<missing node>"

  | Just node <- n ^. #maybe'node = do
      ppNode <- prettyNodeNode node
      let ppConsumedBy =
              maybe mempty
                (\nodeId -> meta $ archivedSC $ text "consumed by:" <-> prettyNodeIdLink nodeId)
                (n ^. #maybe'consumedBy)

      let ppReferencedBy =
            if null (n ^. #referencedBy)
            then mempty
            else meta $ keyword_ "referenced by"
                   <-> fcommasep (map prettyNodeIdLink $ n ^. #referencedBy)

      let ppDisclosedTo =
            if null (n ^. #observingSince)
            then mempty
            else
              meta $ keyword_ "known to (since):"
                <-> fcommasep
                  (map
                    (\patId -> prettyMayParty (patId ^. #maybe'party) <-> parens (prettyTxId $ patId ^. #txId))
                    $ n ^. #observingSince)

      pure
         $ prettyMay "<missing node id>" prettyNodeId (n ^. #maybe'nodeId)
        $$ vcat
             [ ppConsumedBy, ppReferencedBy, ppDisclosedTo
             , arrowright ppNode
             ]

  where
    arrowright p = text "└─>" <-> p
    meta p       = text "│  " <-> p
    archivedSC = annotateSC PredicateSC -- Magenta


prettyPartialTransaction :: PartialTransaction -> M (Doc SyntaxClass)
prettyPartialTransaction partialTransaction = do
  world <- askWorld
  let ppNodes =
           runM (partialTransaction ^. #nodes) world
         $ fmap vsep
         $ mapM (lookupNode >=> prettyNode)
                (partialTransaction ^. #roots)
  pure $ vcat
    [ case partialTransaction ^. #maybe'exerciseContext of
        Nothing -> mempty
        Just exerciseContext ->
          text "Failed exercise"
            <-> parens (prettyMayLocation world (exerciseContext ^. #maybe'exerciseLocation)) <> ":"
            $$ nest 2 (
                keyword_ "exercise"
            <-> prettyMay "<missing template id>"
                  (\tid ->
                      prettyChoiceId world tid (exerciseContext ^. #choiceId))
                  (view #maybe'templateId <$> view #maybe'targetId exerciseContext)
            <-> keyword_ "on"
            <-> prettyMay "<missing>"
                  (prettyContractRef world)
                  (exerciseContext ^. #maybe'targetId)
             $$ keyword_ "with"
             $$ ( nest 2
                $ prettyMay "<missing>"
                    (prettyValue' False 0 world)
                    (exerciseContext ^. #maybe'chosenValue))
            )

   , if null (partialTransaction ^. #roots)
     then mempty
     else text "Sub-transactions:" $$ nest 3 ppNodes
   ]


prettyValue' :: Bool -> Int -> LF.World -> Value -> Doc SyntaxClass
prettyValue' _ _ _ v | Nothing <- v ^. #maybe'sum = text "<missing value>"
prettyValue' showRecordType prec world v | Just sum <- v ^. #maybe'sum = case sum of
  Value'Record rec ->
    maybeParens (prec > precWith) $
      (if showRecordType
       then \fs -> prettyMay "" (prettyDefName world) (rec ^. #maybe'recordId) <-> keyword_ "with" $$ nest 2 fs
       else id)
      (sep (punctuate ";" (map prettyField $ rec ^. #fields)))
  Value'Tuple tuple ->
    braces (sep (punctuate ";" (map prettyField $ tuple ^. #fields)))
  Value'Variant variant ->
        prettyMay "" (\v -> prettyDefName world v <> ":") (variant ^. #maybe'variantId) <> text (variant ^. #constructor)
    <-> prettyMay "<missing value>" (prettyValue' True precHighest world) (variant ^. #maybe'value)
  Value'Enum enum ->
        prettyMay "" (\x -> prettyDefName world x <> ":") (enum ^. #maybe'enumId) <> text (enum ^. #constructor)
  Value'List list ->
    brackets (fcommasep (map (prettyValue' True prec world) $ list ^. #elements))
  Value'ContractId coid -> prettyContractId coid
  Value'Int64 i -> string (show i)
  Value'Decimal ds ->
    case preview stringToDecimal (T.unpack ds) of
      Nothing -> text ds
      Just d  -> string $ review stringToDecimal d
  Value'Text t -> char '"' <> text t <> char '"'
  Value'Timestamp ts -> prettyTimestamp ts
  Value'Party p -> char '\'' <> text p <> char '\''
  Value'Bool True -> text "true"
  Value'Bool False -> text "false"
  Value'Unit{} -> text "{}"
  Value'Date d -> prettyDate d
  Value'Optional o -> case o ^. #maybe'value of
      Nothing -> text "none"
      Just v -> "some " <> prettyValue' True precHighest world v
  Value'Map m -> "Map" <> brackets (fcommasep (map (prettyEntry prec world) $ m ^. #entries))
  Value'Unserializable what -> text what
  where
    prettyField f =
      hang (text (f ^. #label) <-> "=") 2
        (prettyMay "<missing value>" (prettyValue' True precHighest world) (f ^. #maybe'value))
    precWith = 1
    precHighest = 9

prettyEntry :: Int -> LF.World ->  Map'Entry -> Doc SyntaxClass
prettyEntry prec world entry = case entry ^. #maybe'value of
    Just value -> text (entry ^. #key) <> "->" <> prettyValue' True prec world value
    Nothing -> text (entry ^. #key) <> "-> <missing value>"

prettyDate :: Int32 -> Doc a
prettyDate =
    string
  . TF.formatTime TF.defaultTimeLocale "%FT"
  . CP.posixSecondsToUTCTime
  . (24*60*60*)
  . fromIntegral


prettyPackageIdentifier :: PackageIdentifier -> Doc SyntaxClass
prettyPackageIdentifier pId = case pId ^. #maybe'sum of
  Nothing                                    -> mempty
  (Just (PackageIdentifier'Self _))        -> mempty
  (Just (PackageIdentifier'PackageId pid)) -> char '@' <> text pid

prettyDefName :: LF.World -> Identifier -> Doc SyntaxClass
prettyDefName world identifier
  | Just (_modName, defName, mod0) <- lookupModuleFromQualifiedName world (identifier ^. #maybe'package) name
  , Just fp <- LF.moduleSource mod0
  , Just (LF.SourceLoc _mref sline _scol eline _ecol) <- lookupDefLocation mod0 defName =
      linkSC (revealLocationUri fp sline eline) name ppName
  | otherwise =
      ppName
  where
    name = identifier ^. #name
    ppName = text name <> ppPkgId
    ppPkgId = maybe mempty prettyPackageIdentifier (identifier ^. #maybe'package)

prettyChoiceId
  :: LF.World -> Maybe Identifier -> T.Text
  -> Doc SyntaxClass
prettyChoiceId _ Nothing choiceId = text choiceId
prettyChoiceId world (Just identifier) choiceId
  | Just (_modName, defName, mod0) <- lookupModuleFromQualifiedName world (identifier ^. #maybe'package) (identifier ^. #name)
  , Just fp <- LF.moduleSource mod0
  , Just tpl <- NM.lookup (LF.TypeConName [defName]) (LF.moduleTemplates mod0)
  , Just chc <- NM.lookup (LF.ChoiceName choiceId) (LF.tplChoices tpl)
  , Just (LF.SourceLoc _mref sline _scol eline _ecol) <- LF.chcLocation chc =
      linkSC (revealLocationUri fp sline eline) choiceId $ text choiceId
  | otherwise =
      text choiceId

revealLocationUri :: FilePath -> Int -> Int -> T.Text
revealLocationUri fp sline eline =
    "command:daml.revealLocation?"
  <> encodeURI ("[\"file://" <> T.pack fp <> "\", "
  <> TE.show sline <> ", " <> TE.show eline <> "]")
  where
    encodeURI = Network.URI.Encode.encodeText

prettyContractRef :: LF.World -> ContractRef -> Doc SyntaxClass
prettyContractRef world contractRef =
  hsep
  [ prettyContractId (contractRef ^. #contractId)
  , parens (prettyMay "<missing template id>" (prettyDefName world) (contractRef ^. #maybe'templateId))
  ]


-- TABLE VIEW

data NodeInfo = NodeInfo
    { niTemplateId :: Identifier
    , niNodeId :: NodeId
    , niValue :: Value
    , niActive :: Bool
    , niObservers :: S.Set T.Text
    }

data Table = Table
    { tTemplateId :: Identifier
    , tRows :: [NodeInfo]
    }

nodeInfo :: Node -> Maybe NodeInfo
nodeInfo node = do
    create <- node ^. #maybe'create
    niNodeId <- node ^. #maybe'nodeId
    inst <- create ^. #maybe'contractInstance
    niTemplateId <- inst ^. #maybe'templateId
    niValue <- inst ^. #maybe'value
    let niActive = isNothing (node ^. #maybe'consumedBy)
    let niObservers = S.fromList $ node ^.. #observingSince . traverse . #maybe'party . _Just . #party
    pure NodeInfo{..}

groupTables :: [NodeInfo] -> [Table]
groupTables =
    map (uncurry Table)
    . MS.toList
    . MS.map (sortOn (parseNodeId . niNodeId))
    . MS.fromListWith (++)
    . map (\node -> (niTemplateId node, [node]))

renderValue :: LF.World -> [T.Text] -> Value -> (H.Html, H.Html)
renderValue world name value = case value ^? #maybe'record . _Just . #fields of
    Just fields ->
        let (ths, tds) = unzip $ map renderField fields
        in (mconcat ths, mconcat tds)
    Nothing ->
        let th = H.th $ H.text $ T.intercalate "." name
            td = H.td $ H.text $ renderPlain $ prettyValue' True 0 world value
        in (th, td)
    where
        renderField f = renderValue world (name ++ [f ^. #label]) (f ^. #value)

-- templateConName :: Identifier -> LF.Qualified LF.TypeConName
-- templateConName (Identifier mbPkgId (TL.toStrict -> qualName)) = LF.Qualified pkgRef  mdN tpl
--   where (mdN, tpl) = case T.splitOn ":" qualName of
--           [modName, defN] -> (LF.ModuleName (T.splitOn "." modName) , LF.TypeConName (T.splitOn "." defN) )
--           _ -> error "malformed identifier"
--         pkgRef = case mbPkgId of
--                   Just (PackageIdentifier (Just (PackageIdentifierSumPackageId pkgId))) -> LF.PRImport $ LF.PackageId $ TL.toStrict pkgId
--                   Just (PackageIdentifier (Just (PackageIdentifierSumSelf _))) -> LF.PRSelf
--                   Just (PackageIdentifier Nothing) -> error "unidentified package reference"
--                   Nothing -> error "unidentified package reference"

-- labledField :: T.Text -> T.Text -> T.Text
-- labledField fname "" = fname
-- labledField fname label = fname <> "." <> label

-- typeConFieldsNames :: LF.World -> (LF.FieldName, LF.Type) -> [T.Text]
-- typeConFieldsNames world (LF.FieldName fName, LF.TConApp tcn _) = map (labledField fName) (typeConFields tcn world)
-- typeConFieldsNames _ (LF.FieldName fName, _) = [fName]

-- typeConFields :: LF.Qualified LF.TypeConName -> LF.World -> [T.Text]
-- typeConFields qName world = case LF.lookupDataType qName world of
--   Right dataType -> case LF.dataCons dataType of
--     LF.DataRecord re -> concatMap (typeConFieldsNames world) re
--     LF.DataVariant _ -> [""]
--     LF.DataEnum _ -> [""]
--   Left _ -> error "malformed template constructor"

-- renderHeader :: LF.World -> Identifier -> S.Set T.Text -> H.Html
-- renderHeader world identifier parties = H.tr $ mconcat
--             [ foldMap (H.th . (H.div H.! A.class_ "observer") . H.text) parties
--             , H.th "id"
--             , H.th "status"
--             , foldMap (H.th . H.text) (typeConFields (templateConName identifier) world)
--             ]

-- renderRow :: LF.World -> S.Set T.Text -> NodeInfo -> H.Html
-- renderRow world parties NodeInfo{..} =
--     let (_, tds) = renderValue world [] niValue
--         observed party = if party `S.member` niObservers then "X" else "-"
--         active = if niActive then "active" else "archived"
--         row = H.tr H.! A.class_ (H.textValue active) $ mconcat
--             [ foldMap ((H.td H.! A.class_ "disclosure") . H.text . observed) parties
--             , H.td (H.text $ renderPlain $ prettyNodeId niNodeId)
--             , H.td (H.text active)
--             , tds
--             ]
--     in row

-- renderTable :: LF.World -> Table -> H.Html
-- renderTable world Table{..} = H.div H.! A.class_ active $ do
--     let parties = S.unions $ map niObservers tRows
--     H.h1 $ renderPlain $ prettyDefName world tTemplateId
--     let rows = map (renderRow world parties) tRows
--     let header = renderHeader world tTemplateId parties
--     H.table $ header <> mconcat rows
--     where
--         active = if any niActive tRows then "active" else "archived"

-- renderTableView :: LF.World -> ScenarioResult -> Maybe H.Html
-- renderTableView world ScenarioResult{..} =
--     let nodes = mapMaybe nodeInfo (V.toList scenarioResultNodes)
--         tables = groupTables nodes
--     in if null nodes then Nothing else Just $ H.div H.! A.class_ "table" $ foldMap (renderTable world) tables

-- renderTransactionView :: LF.World -> ScenarioResult -> H.Html
-- renderTransactionView world res =
--     let doc = prettyScenarioResult world res
--     in H.div H.! A.class_ "da-code transaction" $ Pretty.renderHtml 128 doc

-- renderScenarioResult :: LF.World -> ScenarioResult -> T.Text
-- renderScenarioResult world res = TL.toStrict $ Blaze.renderHtml $ do
--     H.docTypeHtml $ do
--         H.head $ do
--             H.style $ H.text Pretty.highlightStylesheet
--             H.style $ H.text stylesheet
--             H.script "" H.! A.src "$webviewSrc"
--         let tableView = renderTableView world res
--         let transView = renderTransactionView world res
--         case tableView of
--             Nothing -> H.body transView
--             Just tbl -> H.body H.! A.class_ "hide_archived hide_transaction" $ do
--                 H.div $ do
--                     H.button H.! A.onclick "toggle_view();" $ do
--                         H.span H.! A.class_ "table" $ H.text "Show transaction view"
--                         H.span H.! A.class_ "transaction" $ H.text "Show table view"
--                     H.text " "
--                     H.span H.! A.class_ "table" $ do
--                         H.input H.! A.type_ "checkbox" H.! A.id "show_archived" H.! A.onchange "show_archived_changed();"
--                         H.label H.! A.for "show_archived" $ "Show archived"
--                 tbl
--                 transView

stylesheet :: T.Text
stylesheet = T.unlines
  [ "table, th, td {"
  , "  border: 1px solid;"
  , "  border-collapse: collapse;"
  , "}"
  , "th, td {"
  , "  padding-left: 2px;"
  , "  padding-right: 2px;"
  , "}"
  , "body.hide_archived .archived {"
  , "  display: none;"
  , "}"
  , "body.hide_table .table {"
  , "  display: none;"
  , "}"
  , "body.hide_transaction .transaction {"
  , "  display: none;"
  , "}"
  , "tr.archived td {"
  , "  text-decoration: line-through;"
  , "}"
  , "td.disclosure {"
  , "  max-width: 1em;"
  , "  text-align: center;"
  , "}"
  , "tr.archived td.disclosure {"
  , "  text-decoration: none;"
  , "}"
  , "th {"
  , "  vertical-align: bottom;"
  , "}"
  , "div.observer {"
  , "  max-width: 1em;"
  , "  writing-mode: vertical-rl;"
  , "  transform: rotate(180deg);"
  , "}"
  ]
