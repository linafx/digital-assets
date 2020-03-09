package com.daml.ledger.participant.state.kvutils.api

import java.util.UUID

import akka.stream.QueueOfferResult.Enqueued
import akka.stream.scaladsl.{Keep, RestartSink, Sink, Source, SourceQueueWithComplete}
import akka.stream.{Materializer, OverflowStrategy}
import com.daml.ledger.participant.state.v1.{ParticipantId, SubmissionResult}
import com.daml.ledger.participant.state.kvutils.DamlKvutils.DamlSubmissionBatch
import com.digitalasset.ledger.api.health.HealthStatus
import com.google.protobuf.ByteString

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.JavaConverters._

class BatchingLedgerWriter(val writer: LedgerWriter, implicit val materializer: Materializer)
    extends LedgerWriter {
  val maxBatchSize = 8 * 1024 * 1024 // 8MB
  val maxWaitDuration = 100 milliseconds
  val numParallelSends = 16
  val queue: SourceQueueWithComplete[(String, Array[Byte])] =
    Source
      .queue(128, OverflowStrategy.backpressure)
      .groupedWeightedWithin(maxBatchSize, maxWaitDuration) {
        case (_, envelope: Array[Byte]) =>
          envelope.length
      }
      .toMat(sendSink)(Keep.left)
      .run

  val sendSink =
    RestartSink.withBackoff(maxWaitDuration, maxWaitDuration, 0.5) { () =>
      Sink.foreachAsync(numParallelSends)(sendBatch)
    }

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
    writer
      .commit(UUID.randomUUID().toString, builder.build.toByteArray)
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
