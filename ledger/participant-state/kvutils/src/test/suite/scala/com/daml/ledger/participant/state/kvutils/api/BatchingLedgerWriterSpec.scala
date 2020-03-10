// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.participant.state.kvutils.api

import com.daml.ledger.participant.state.kvutils.DamlKvutils.DamlSubmissionBatch
import com.daml.ledger.participant.state.kvutils.Envelope
import com.daml.ledger.participant.state.v1
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.digitalasset.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.google.protobuf.ByteString
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers
import org.mockito.Mockito.{times, _}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.Future
import scala.concurrent.duration._

class BatchingLedgerWriterSpec
    extends AsyncWordSpec
    with MockitoSugar
    with AkkaBeforeAndAfterAll
    with Matchers {

  private def createWriter(captor: Option[ArgumentCaptor[Array[Byte]]] = None): LedgerWriter = {
    val writer = mock[LedgerWriter]
    when(
      writer.commit(
        ArgumentMatchers.anyString(),
        captor.map(_.capture()).getOrElse(ArgumentMatchers.any[Array[Byte]]())))
      .thenReturn(Future.successful(SubmissionResult.Acknowledged))
    when(writer.participantId).thenReturn(v1.ParticipantId.assertFromString("test-participant"))
    writer
  }

  "BatchingLedgerWriter" should {
    "construct batch within maxWait" in {
      val batchCaptor: ArgumentCaptor[Array[Byte]] = ArgumentCaptor.forClass(classOf[Array[Byte]])
      val mockWriter = createWriter(Some(batchCaptor))
      val batchingWriter = new BatchingLedgerWriter(
        writer = mockWriter,
        maxQueueSize = 128,
        maxBatchSizeBytes = 1024,
        maxWaitDuration = 50.millis,
        maxParallelism = 1)

      val expectedBatch =
        Envelope
          .enclose(
            DamlSubmissionBatch.newBuilder
              .addSubmissions(DamlSubmissionBatch.CorrelatedSubmission.newBuilder
                .setCorrelationId("test")
                .setSubmission(ByteString.copyFrom(Array[Byte](1, 2, 3))))
              .build)
          .toByteArray

      for {
        res <- batchingWriter.commit("test", Array[Byte](1, 2, 3))
        _ <- Future { Thread.sleep(2 * batchingWriter.maxWaitDuration.toMillis); }
      } yield {
        verify(mockWriter, times(1))
          .commit(ArgumentMatchers.anyString(), ArgumentMatchers.eq(expectedBatch))

        res should be(SubmissionResult.Acknowledged)
      }
    }

  }

  /*
    "submit a transaction" in {
      val transactionCaptor = ArgumentCaptor.forClass(classOf[Array[Byte]])
      val writer = createWriter(Some(transactionCaptor))
      val instance = new KeyValueParticipantStateWriter(writer)
      val recordTime = newRecordTime()

      instance.submitTransaction(
        submitterInfo(recordTime, aParty),
        transactionMeta(recordTime),
        anEmptyTransaction)
      verify(writer, times(1)).commit(anyString(), any[Array[Byte]]())
      verifyEnvelope(transactionCaptor.getValue)(_.hasTransactionEntry)
    }

    "upload a package" in {
      val packageUploadCaptor = ArgumentCaptor.forClass(classOf[Array[Byte]])
      val writer = createWriter(Some(packageUploadCaptor))
      val instance = new KeyValueParticipantStateWriter(writer)

      instance.uploadPackages(aSubmissionId, List.empty, sourceDescription = None)
      verify(writer, times(1)).commit(anyString(), any[Array[Byte]]())
      verifyEnvelope(packageUploadCaptor.getValue)(_.hasPackageUploadEntry)
    }

    "submit a configuration" in {
      val configurationCaptor = ArgumentCaptor.forClass(classOf[Array[Byte]])
      val writer = createWriter(Some(configurationCaptor))
      val instance = new KeyValueParticipantStateWriter(writer)

      instance.submitConfiguration(newRecordTime().addMicros(10000), aSubmissionId, aConfiguration)
      verify(writer, times(1)).commit(anyString(), any[Array[Byte]]())
      verifyEnvelope(configurationCaptor.getValue)(_.hasConfigurationSubmission)
    }

    "allocate a party without hint" in {
      val partyAllocationCaptor = ArgumentCaptor.forClass(classOf[Array[Byte]])
      val writer = createWriter(Some(partyAllocationCaptor))
      val instance = new KeyValueParticipantStateWriter(writer)

      instance.allocateParty(hint = None, displayName = None, aSubmissionId)
      verify(writer, times(1)).commit(anyString(), any[Array[Byte]]())
      verifyEnvelope(partyAllocationCaptor.getValue)(_.hasPartyAllocationEntry)
    }


   */

  /*
  private def verifyEnvelope(written: Array[Byte])(
      assertion: DamlSubmission => Boolean): Assertion =
    Envelope.openSubmission(written) match {
      case Right(value) => assert(assertion(value) === true)
      case _ => fail()
    }

  private val aParty = Ref.Party.assertFromString("aParty")

  private val anEmptyTransaction: Transaction.AbsTransaction =
    GenTransaction(HashMap.empty, ImmArray.empty)

  private val aSubmissionId: SubmissionId =
    Ref.LedgerString.assertFromString(UUID.randomUUID().toString)

  private val aConfiguration: Configuration = Configuration(1, TimeModel.reasonableDefault)

  private def createWriter(captor: Option[ArgumentCaptor[Array[Byte]]] = None): LedgerWriter = {
    val writer = mock[LedgerWriter]
    when(writer.commit(anyString(), captor.map(_.capture()).getOrElse(any[Array[Byte]]())))
      .thenReturn(Future.successful(SubmissionResult.Acknowledged))
    when(writer.participantId).thenReturn(v1.ParticipantId.assertFromString("test-participant"))
    writer
  }

  private def submitterInfo(rt: Timestamp, party: Ref.Party) = SubmitterInfo(
    submitter = party,
    applicationId = Ref.LedgerString.assertFromString("tests"),
    commandId = Ref.LedgerString.assertFromString("X"),
    maxRecordTime = rt.addMicros(Duration.ofSeconds(10).toNanos / 1000)
  )

  private def transactionMeta(let: Timestamp) = TransactionMeta(
    ledgerEffectiveTime = let,
    workflowId = Some(Ref.LedgerString.assertFromString("tests")),
    submissionSeed = Some(
      crypto.Hash.assertFromString(
        "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")),
    optUsedPackages = Some(Set.empty)
  )

  private def newRecordTime(): Timestamp =
    Timestamp.assertFromInstant(Clock.systemUTC().instant())*/
}
