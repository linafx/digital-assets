// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.transaction.dump

import com.daml.auth.TokenHolder
import com.daml.ledger.client.configuration.{
  CommandClientConfiguration,
  LedgerClientConfiguration,
  LedgerIdRequirement
}
import com.typesafe.scalalogging.StrictLogging
import scalaz.syntax.show._

object Main extends StrictLogging {

  object ErrorCodes {
    val Ok = 0
    val InvalidUsage = 100
    val StartupError = 101
  }

  def main(args: Array[String]): Unit =
    Config.parseConfig(args) match {
      case Some(config) =>
        main(config)
      case None =>
        // error is printed out by scopt
        sys.exit(ErrorCodes.InvalidUsage)
    }

  private def main(config: Config): Unit = {
    logger.info(config.shows)

    val tokenHolder = config.accessTokenFile.map(new TokenHolder(_))

    val clientConfig = LedgerClientConfiguration(
      applicationId = "TransactionDump",
      ledgerIdRequirement = LedgerIdRequirement("", enabled = false),
      commandClient = CommandClientConfiguration.default,
      sslContext = None,
      token = tokenHolder.flatMap(_.token),
    )

    logger.info(s"Client config: $clientConfig")

  }
}
