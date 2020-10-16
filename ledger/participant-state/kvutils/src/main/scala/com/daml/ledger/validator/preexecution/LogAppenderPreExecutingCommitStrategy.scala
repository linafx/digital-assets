// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.validator.preexecution

import com.daml.ledger.participant.state.kvutils.DamlKvutils.{DamlLogEntryId}
import com.daml.ledger.participant.state.kvutils.{DamlKvutils, Envelope, KeyValueCommitting}
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.{StateKeySerializationStrategy, StateSerializationStrategy/*, inParallel*/}
//import com.google.protobuf.ByteString
import com.daml.lf.data.SeqTrack

import scala.concurrent.{ExecutionContext, Future}

final class LogAppenderPreExecutingCommitStrategy(
    keySerializationStrategy: StateKeySerializationStrategy,
) extends PreExecutingCommitStrategy[RawKeyValuePairsWithLogEntry] {
  private val stateSerializationStrategy = new StateSerializationStrategy(keySerializationStrategy)

  override def generateWriteSets(
      participantId: ParticipantId,
      logEntryId: DamlLogEntryId,
      inputState: Map[DamlKvutils.DamlStateKey, Option[DamlKvutils.DamlStateValue]],
      preExecutionResult: KeyValueCommitting.PreExecutionResult,
      t: Option[SeqTrack] = None
  )(implicit executionContext: ExecutionContext)
    : Future[PreExecutionCommitResult[RawKeyValuePairsWithLogEntry]] = {
    t.foreach(_.startStep("serialize key value pairs !"))
    val serializedSuccessKeyValuePairs = stateSerializationStrategy.serializeState(preExecutionResult.stateUpdates)
    t.foreach(_.startStep("to bytestring"))
    val serializedId = logEntryId.toByteString
    t.foreach(_.startStep("enclosing"))
    val serializedSuccessLogEntry = Envelope.enclose(preExecutionResult.successfulLogEntry)
    val serializedOutOfTimeBoundsLogEntry = Envelope.enclose(preExecutionResult.outOfTimeBoundsLogEntry)
    t.foreach(_.startStep("build result"))
    Future.successful(PreExecutionCommitResult(
      successWriteSet = RawKeyValuePairsWithLogEntry(
        serializedSuccessKeyValuePairs,
        serializedId,
        serializedSuccessLogEntry,
      ),
      outOfTimeBoundsWriteSet = RawKeyValuePairsWithLogEntry(
        Seq.empty,
        serializedId,
        serializedOutOfTimeBoundsLogEntry,
      ),
      // We assume updates for a successful transaction must be visible to every participant for
      // public ledgers.
      involvedParticipants = Set.empty,
    ))

//    for {
//      (
//        serializedSuccessKeyValuePairs,
//        (serializedSuccessLogEntryPair, serializedOutOfTimeBoundsLogEntryPair),
//      ) <- inParallel(
//        Future(stateSerializationStrategy.serializeState(preExecutionResult.stateUpdates)),
//        Future(logEntryId.toByteString).flatMap(
//          serializedId =>
//            inParallel(
//              logEntryToKeyValuePairs(serializedId, preExecutionResult.successfulLogEntry),
//              logEntryToKeyValuePairs(serializedId, preExecutionResult.outOfTimeBoundsLogEntry),
//          )),
//      )
//    } yield
//      PreExecutionCommitResult(
//        successWriteSet = RawKeyValuePairsWithLogEntry(
//          serializedSuccessKeyValuePairs,
//          serializedSuccessLogEntryPair._1,
//          serializedSuccessLogEntryPair._2,
//        ),
//        outOfTimeBoundsWriteSet = RawKeyValuePairsWithLogEntry(
//          Seq.empty,
//          serializedOutOfTimeBoundsLogEntryPair._1,
//          serializedOutOfTimeBoundsLogEntryPair._2,
//        ),
//        // We assume updates for a successful transaction must be visible to every participant for
//        // public ledgers.
//        involvedParticipants = Set.empty,
//      )
  }

//  private def logEntryToKeyValuePairs(
//      logEntryId: ByteString,
//      logEntry: DamlLogEntry,
//  )(implicit executionContext: ExecutionContext): Future[(Bytes, Bytes)] =
//    Future(logEntryId -> Envelope.enclose(logEntry))
}
