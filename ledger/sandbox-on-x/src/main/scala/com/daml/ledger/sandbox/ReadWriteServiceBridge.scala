package com.daml.ledger.sandbox

import java.util.UUID
import java.util.concurrent.{CompletableFuture, CompletionStage}

import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import com.daml.daml_lf_dev.DamlLf.Archive
import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.v1._
import com.daml.lf.data.Time
import com.daml.lf.data.Time.Timestamp
import com.google.common.primitives.Longs
import io.grpc.Status

case class ReadWriteServiceBridge(
    participantId: ParticipantId,
    ledgerId: LedgerId,
)(implicit mat: Materializer)
    extends ReadService
    with WriteService
    with AutoCloseable {
  import ReadWriteServiceBridge._

  val outputBufferSize = 200

  override def submitTransaction(
      submitterInfo: SubmitterInfo,
      transactionMeta: TransactionMeta,
      transaction: SubmittedTransaction,
      estimatedInterpretationCost: Long,
  ): CompletionStage[SubmissionResult] =
    submit(
      Submission.Transaction(
        submitterInfo = submitterInfo,
        transactionMeta = transactionMeta,
        transaction = transaction,
        estimatedInterpretationCost = estimatedInterpretationCost,
      )
    )

  override def submitConfiguration(
      maxRecordTime: Time.Timestamp,
      submissionId: SubmissionId,
      config: Configuration,
  ): CompletionStage[SubmissionResult] =
    submit(
      Submission.Config(
        maxRecordTime = maxRecordTime,
        submissionId = submissionId,
        config = config,
      )
    )

  override def currentHealth(): HealthStatus = HealthStatus.healthy // FIXME

  override def allocateParty(
      hint: Option[Party],
      displayName: Option[String],
      submissionId: SubmissionId,
  ): CompletionStage[SubmissionResult] =
    submit(
      Submission.AllocateParty(
        hint = hint,
        displayName = displayName,
        submissionId = submissionId,
      )
    )

  override def uploadPackages(
      submissionId: SubmissionId,
      archives: List[Archive],
      sourceDescription: Option[String],
  ): CompletionStage[SubmissionResult] =
    submit(
      Submission.UploadPackages(
        submissionId = submissionId,
        archives = archives,
        sourceDescription = sourceDescription,
      )
    )

  override def prune(
      pruneUpToInclusive: Offset,
      submissionId: SubmissionId,
  ): CompletionStage[PruningResult] =
    CompletableFuture.completedFuture(
      PruningResult.NotPruned(Status.UNIMPLEMENTED)
    ) // FIXME Pruning not supported

  override def getLedgerInitialConditions(): Source[LedgerInitialConditions, NotUsed] =
    Source.single(
      LedgerInitialConditions(
        ledgerId = ledgerId,
        config = Configuration(
          generation = 1L,
          timeModel = TimeModel.reasonableDefault,
          maxDeduplicationTime = java.time.Duration.ofSeconds(30),
        ),
        initialRecordTime = Timestamp.now(),
      )
    )

  var stateUpdatesWasCalledAlready = false
  override def stateUpdates(beginAfter: Option[Offset]): Source[(Offset, Update), NotUsed] = {
    println(s"get updates from $beginAfter")
    // FIXME for PoC purposes:
    //   no beginAfter supported
    //   neither multiple subscriptions
    //   neither bootstrapping the brigde from indexer persistence
    synchronized {
      if (stateUpdatesWasCalledAlready)
        throw new IllegalStateException("not allowed to call this twice")
      else stateUpdatesWasCalledAlready = true
    }
    queueSource
  }

  val (queue: BoundedSourceQueue[Submission], queueSource: Source[(Offset, Update), NotUsed]) =
    Source
      .queue[Submission](bufferSize = outputBufferSize)
      .zipWithIndex
      .map { case (submission, index) =>
        (toOffset(index), successMapper(submission, index, participantId))
      }
      .preMaterialize()

  private def submit(submission: Submission): CompletionStage[SubmissionResult] =
    toSubmissionResult(queue.offer(submission))

  override def close(): Unit = {
    queue.complete()
  } // TODO is this ok?
}

object ReadWriteServiceBridge {
  trait Submission
  object Submission {
    case class Transaction(
        submitterInfo: SubmitterInfo,
        transactionMeta: TransactionMeta,
        transaction: SubmittedTransaction,
        estimatedInterpretationCost: Long,
    ) extends Submission
    case class Config(
        maxRecordTime: Time.Timestamp,
        submissionId: SubmissionId,
        config: Configuration,
    ) extends Submission
    case class AllocateParty(
        hint: Option[Party],
        displayName: Option[String],
        submissionId: SubmissionId,
    ) extends Submission
    case class UploadPackages(
        submissionId: SubmissionId,
        archives: List[Archive],
        sourceDescription: Option[String],
    ) extends Submission
  }

  def successMapper(s: Submission, index: Long, participantId: ParticipantId): Update = s match {
    case s: Submission.AllocateParty =>
      val party = s.hint.getOrElse(UUID.randomUUID().toString)
      Update.PartyAddedToParticipant(
        party = party.asInstanceOf[Party],
        displayName = s.displayName.getOrElse(party),
        participantId = participantId,
        recordTime = Time.Timestamp.now(),
        submissionId = Some(s.submissionId),
      )

    case s: Submission.Config =>
      Update.ConfigurationChanged(
        recordTime = Time.Timestamp.now(),
        submissionId = s.submissionId,
        participantId = participantId,
        newConfiguration = s.config,
      )

    // TODO check the hash against the content
    case s: Submission.UploadPackages =>
      Update.PublicPackageUpload(
        archives = s.archives,
        sourceDescription = s.sourceDescription,
        recordTime = Time.Timestamp.now(),
        submissionId = Some(s.submissionId),
      )

    case s: Submission.Transaction =>
      Update.TransactionAccepted(
        optSubmitterInfo = Some(s.submitterInfo),
        transactionMeta = s.transactionMeta,
        transaction = s.transaction.asInstanceOf[CommittedTransaction],
        transactionId = index.toString.asInstanceOf[TransactionId], // FIXME BOOOOOO
        recordTime = Time.Timestamp.now(),
        divulgedContracts = Nil, // TODO is this ok?
        blindingInfo = None, // TODO is this ok?
      )
  }

  def toOffset(index: Long): Offset = Offset.fromByteArray(Longs.toByteArray(index))

  def toSubmissionResult(queueOfferResult: QueueOfferResult): CompletableFuture[SubmissionResult] =
    CompletableFuture.completedFuture(
      queueOfferResult match {
        case QueueOfferResult.Enqueued => SubmissionResult.Acknowledged
        case QueueOfferResult.Dropped => SubmissionResult.Overloaded
        case QueueOfferResult.Failure(throwable) =>
          SubmissionResult.InternalError(throwable.getMessage)
        case QueueOfferResult.QueueClosed =>
          SubmissionResult.InternalError("Service is shutting down.")
      }
    )
}
