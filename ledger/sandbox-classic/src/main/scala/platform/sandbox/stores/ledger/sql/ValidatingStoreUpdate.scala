package com.daml.platform.sandbox.stores.ledger.sql

import com.daml.ledger.participant.state.v1.{Offset, Update}
import com.daml.logging.LoggingContext
import com.daml.metrics.Metrics
import com.daml.platform.store.dao.DbDispatcher
import com.daml.platform.store.dao.events.{ContractsTable, PostCommitValidation}

import scala.concurrent.Future

object ValidatingStoreUpdate {

  def backedBy(
      dbDispatcher: DbDispatcher,
      validatePartyAllocation: Boolean,
      loggingContext: LoggingContext,
      metrics: Metrics,
  )(
      storeUpdate: (Offset, Update) => Future[Unit]
  ): (Offset, Update) => Future[Unit] = {
    import scala.concurrent.ExecutionContext.Implicits.global // TODO maybe use a different one?
    val postCommitValidator =
      new PostCommitValidation.BackedBy(ContractsTable, validatePartyAllocation)
    (offset, update) => {
      update match {
        case u: Update.TransactionAccepted =>
          dbDispatcher
            .executeSql(metrics.daml.index.db.storeTransactionDbMetrics) { // FIXME in final version probably on the same pool backed by the same dispatcher, and with it's own metrics
              implicit connection =>
                postCommitValidator.validate(
                  u.transaction,
                  u.transactionMeta.ledgerEffectiveTime.toInstant,
                  Set.empty,
                ) // Set can be empty because it is always Nil in sandbox-classic
            }(loggingContext)
            .flatMap {
              case None => storeUpdate(offset, update)
              case Some(rejectionReason) =>
                storeUpdate(
                  offset,
                  Update.CommandRejected(
                    recordTime = u.recordTime,
                    submitterInfo =
                      u.optSubmitterInfo.get, // TODO ouch! ...but: specific for this integration: we will always have a submitter info for a sandbox-classic setup
                    reason = rejectionReason,
                  ),
                )
            }

        // TODO add intricate logic for configuration checking, located in JdbcLedgerDao.storeConfigurationEntry and original SqlLedger.publishConfiguration

        case u => storeUpdate(offset, u)
      }
    }
  }

}
