// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.indexer

import akka.stream._
import akka.stream.scaladsl.Sink
import com.daml.ledger.api.domain
import com.daml.ledger.api.domain.ParticipantId
import com.daml.ledger.participant.state.v1._
import com.daml.ledger.resources.{Resource, ResourceContext, ResourceOwner}
import com.daml.lf.data.Ref
import com.daml.logging.{ContextualizedLogger, LoggingContext}
import com.daml.metrics.Metrics
import com.daml.platform.common
import com.daml.platform.common.MismatchException
import com.daml.platform.configuration.ServerRole
import com.daml.platform.indexer.poc.PoCIndexerFactory
import com.daml.platform.store.FlywayMigrations
import com.daml.platform.store.dao.events.{CompressionStrategy, LfValueTranslation}
import com.daml.platform.store.dao.{JdbcLedgerDao, LedgerDao}

import scala.concurrent.{ExecutionContext, Future}

object JdbcIndexer {

  private[daml] final class Factory private[indexer] (
      config: IndexerConfig,
      readService: ReadService,
      metrics: Metrics,
      ledgerDaoOwner: ResourceOwner[LedgerDao],
      flywayMigrations: FlywayMigrations,
      lfValueTranslationCache: LfValueTranslation.Cache,
  )(implicit materializer: Materializer, loggingContext: LoggingContext) {

    private[daml] def this(
        serverRole: ServerRole,
        config: IndexerConfig,
        readService: ReadService,
        servicesExecutionContext: ExecutionContext,
        metrics: Metrics,
        lfValueTranslationCache: LfValueTranslation.Cache,
    )(implicit materializer: Materializer, loggingContext: LoggingContext) =
      this(
        config,
        readService,
        metrics,
        JdbcLedgerDao.writeOwner(
          serverRole,
          config.jdbcUrl,
          config.databaseConnectionPoolSize,
          config.eventsPageSize,
          servicesExecutionContext,
          metrics,
          lfValueTranslationCache,
          jdbcAsyncCommits = true,
          enricher = None,
        ),
        new FlywayMigrations(config.jdbcUrl),
        lfValueTranslationCache,
      )

    private val logger = ContextualizedLogger.get(this.getClass)

    def validateSchema()(implicit
        resourceContext: ResourceContext
    ): Future[ResourceOwner[Indexer]] =
      flywayMigrations
        .validate()
        .flatMap(_ => initialized(resetSchema = false))(resourceContext.executionContext)

    def migrateSchema(
        allowExistingSchema: Boolean
    )(implicit resourceContext: ResourceContext): Future[ResourceOwner[Indexer]] =
      flywayMigrations
        .migrate(allowExistingSchema)
        .flatMap(_ => initialized(resetSchema = false))(resourceContext.executionContext)

    def resetSchema(): Future[ResourceOwner[Indexer]] = initialized(resetSchema = true)

    private def initialized(resetSchema: Boolean): Future[ResourceOwner[Indexer]] =
      Future.successful(
        for {
          ledgerDao <- ledgerDaoOwner
          _ <-
            if (resetSchema) {
              ResourceOwner.forFuture(() => ledgerDao.reset())
            } else {
              ResourceOwner.unit
            }
          _ <- initializeLedger(ledgerDao)()
          // TODO we defer initialisation to original code for now, from here PoC ledger factoring starts
          // TODO until here mutable initialisation is concluded, from here read only initialisation ATM
          indexer <- PoCIndexerFactory(
            jdbcUrl = config.jdbcUrl,
            participantId = config.participantId,
            translation = new LfValueTranslation(
              cache = lfValueTranslationCache,
              metrics = metrics,
              enricherO = None,
              loadPackage = (_, _) => Future.successful(None),
            ),
            compressionStrategy =
              if (config.enableCompression) CompressionStrategy.allGZIP(metrics)
              else CompressionStrategy.none(metrics),
            mat = materializer,
            inputMappingParallelism = config.inputMappingParallelism,
            ingestionParallelism = config.ingestionParallelism,
            submissionBatchSize = config.submissionBatchSize,
            tailingRateLimitPerSecond = config.tailingRateLimitPerSecond,
            batchWithinMillis = config.batchWithinMillis,
            runStageUntil = config.runStageUntil,
            metrics = metrics,
          )
        } yield indexer
      )

    private def initializeLedger(dao: LedgerDao)(): ResourceOwner[Option[Offset]] =
      new ResourceOwner[Option[Offset]] {
        override def acquire()(implicit context: ResourceContext): Resource[Option[Offset]] =
          Resource.fromFuture(for {
            initialConditions <- readService.getLedgerInitialConditions().runWith(Sink.head)
            existingLedgerId <- dao.lookupLedgerId()
            providedLedgerId = domain.LedgerId(initialConditions.ledgerId)
            _ <- existingLedgerId.fold(initializeLedgerData(providedLedgerId, dao))(
              checkLedgerIds(_, providedLedgerId)
            )
            _ <- initOrCheckParticipantId(dao)
            initialLedgerEnd <- dao.lookupInitialLedgerEnd()
          } yield initialLedgerEnd)
      }

    private def checkLedgerIds(
        existingLedgerId: domain.LedgerId,
        providedLedgerId: domain.LedgerId,
    ): Future[Unit] =
      if (existingLedgerId == providedLedgerId) {
        logger.info(s"Found existing ledger with ID: $existingLedgerId")
        Future.unit
      } else {
        Future.failed(new MismatchException.LedgerId(existingLedgerId, providedLedgerId))
      }

    private def initializeLedgerData(
        providedLedgerId: domain.LedgerId,
        ledgerDao: LedgerDao,
    ): Future[Unit] = {
      logger.info(s"Initializing ledger with ID: $providedLedgerId")
      ledgerDao.initializeLedger(providedLedgerId)
    }

    private def initOrCheckParticipantId(
        dao: LedgerDao
    )(implicit resourceContext: ResourceContext): Future[Unit] = {
      val id = ParticipantId(Ref.ParticipantId.assertFromString(config.participantId))
      dao
        .lookupParticipantId()
        .flatMap(
          _.fold(dao.initializeParticipantId(id)) {
            case `id` =>
              Future.successful(logger.info(s"Found existing participant id '$id'"))
            case retrievedLedgerId =>
              Future.failed(new common.MismatchException.ParticipantId(retrievedLedgerId, id))
          }
        )(resourceContext.executionContext)
    }

  }
}

class SubscriptionIndexFeedHandle(val killSwitch: KillSwitch, override val completed: Future[Unit])
    extends IndexFeedHandle
