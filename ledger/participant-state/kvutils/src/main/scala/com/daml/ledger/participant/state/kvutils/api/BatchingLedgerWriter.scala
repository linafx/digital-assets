package com.daml.ledger.participant.state.kvutils.api

import java.util.UUID

import akka.NotUsed
import akka.stream.QueueOfferResult.Enqueued
import akka.stream.scaladsl.{Keep, RestartSink, Sink, Source, SourceQueueWithComplete}
import akka.stream.{Materializer, OverflowStrategy}
import com.daml.ledger.participant.state.kvutils.DamlKvutils.DamlSubmissionBatch
import com.daml.ledger.participant.state.kvutils.Envelope
import com.daml.ledger.participant.state.v1.{ParticipantId, SubmissionResult}
import com.digitalasset.ledger.api.health.HealthStatus
import com.google.protobuf.ByteString

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._

class BatchingLedgerWriter(
    val writer: LedgerWriter,
    val maxQueueSize: Int,
    val maxBatchSizeBytes: Long,
    val maxWaitDuration: FiniteDuration,
    val maxParallelism: Int)(implicit val materializer: Materializer)
    extends LedgerWriter {

  private def submissionCost(submission: (String, Array[Byte])): Long =
    submission._2.length.toLong

  private val sendSink: Sink[Seq[(String, Array[Byte])], NotUsed] =
    // FIXME(JM): Parameters need tuning
    RestartSink.withBackoff(100.millis, 1.seconds, 0.2) { () =>
      Sink.foreachAsync(maxParallelism)(sendBatch)
    }

  //@SuppressWarnings(Array("org.wartremover.warts.Any"))
  val queue: SourceQueueWithComplete[(String, Array[Byte])] =
    Source
      .queue(maxQueueSize, OverflowStrategy.dropNew)
      .groupedWeightedWithin(maxBatchSizeBytes, maxWaitDuration)(submissionCost)
      .toMat(sendSink)(Keep.left[SourceQueueWithComplete[(String, Array[Byte])], NotUsed])
      .run

  // TODO(JM): We need to keep repeating sending of the batch if it fails. Use Sink.withBackoff?
  private def sendBatch(batch: Seq[(String, Array[Byte])]): Future[Unit] = {
    val builder = DamlSubmissionBatch.newBuilder
    builder.addAllSubmissions(
      batch.map {
        case (correlationId, envelope) =>
          DamlSubmissionBatch.CorrelatedSubmission.newBuilder
            .setCorrelationId(correlationId)
            .setSubmission(ByteString.copyFrom(envelope))
            .build
      }.asJava
    )

    val envelope = Envelope.enclose(builder.build)
    writer
      .commit(UUID.randomUUID().toString, envelope.toByteArray)
      .map(_ => ())(materializer.executionContext) // FIXME(JM): wrong ec?
  }

  override def commit(correlationId: String, envelope: Array[Byte]): Future[SubmissionResult] =
    queue
      .offer(correlationId -> envelope)
      .map {
        case Enqueued => SubmissionResult.Acknowledged
        case _ => SubmissionResult.Overloaded // FIXME(JM)
      }(materializer.executionContext) // FIXME(JM): what ec?

  override def participantId: ParticipantId = writer.participantId

  override def currentHealth(): HealthStatus = writer.currentHealth()
}
