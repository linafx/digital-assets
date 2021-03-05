package com.daml.ledger.sandbox

import akka.stream.Materializer
import com.daml.ledger.participant.state.kvutils.app.{Config, LedgerFactory, ParticipantConfig}
import com.daml.ledger.resources.ResourceOwner
import com.daml.lf.engine.Engine
import com.daml.logging.LoggingContext
import com.daml.platform.configuration.LedgerConfiguration
import scopt.OptionParser

object BridgeLedgerFactory extends LedgerFactory[ReadWriteServiceBridge, Unit] {

  override final def readWriteServiceOwner(
      config: Config[Unit],
      participantConfig: ParticipantConfig,
      engine: Engine,
  )(implicit
      materializer: Materializer,
      loggingContext: LoggingContext,
  ): ResourceOwner[ReadWriteServiceBridge] =
    ResourceOwner.forCloseable(() => ReadWriteServiceBridge(
      participantId = participantConfig.participantId,
      ledgerId = config.ledgerId,
    ))

  override def ledgerConfig(config: Config[Unit]): LedgerConfiguration =
    LedgerConfiguration.defaultLocalLedger

  override def extraConfigParser(parser: OptionParser[Config[Unit]]): Unit = ()

  override val defaultExtraConfig: Unit = ()
}
