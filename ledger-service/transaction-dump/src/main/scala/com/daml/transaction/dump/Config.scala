// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.transaction.dump

import com.daml.ledger.api.refinements.ApiTypes.Party
import scalaz.{OneAnd, Show}
import scopt.{Read, RenderingMode}

import scala.concurrent.duration.{Duration, FiniteDuration}

// TODO(Leo): consider merging it with com.daml.http.Config or factoring out the common parts
final case class Config(
    ledgerHost: String,
    ledgerPort: Int,
    parties: OneAnd[Set, Party],
    messageRateInterval: FiniteDuration,
)

object Config {
  val Empty = Config(
    ledgerHost = "",
    ledgerPort = -1,
    parties = OneAnd(Party(""), Set.empty),
    messageRateInterval = FiniteDuration(0L, java.util.concurrent.TimeUnit.NANOSECONDS))

  implicit val showInstance: Show[Config] = Show.shows { c =>
    s"""Config(ledgerHost="${c.ledgerHost}, ledgerPort=${c.ledgerPort}, parties=${c.parties.toString}, messageRateInterval=${c.messageRateInterval})"""
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private implicit def nonEmptySetRead[A: Read]: Read[OneAnd[Set, A]] = Read.reads { (s: String) =>
    import scalaz.std.iterable._
    import scalaz.syntax.foldable._

    def errorMsg = s"Expected a non-empty sequence of unique elements, got '$s'"

    Read
      .seqRead[A]
      .map {
        case Seq() =>
          throw new IllegalArgumentException(errorMsg)
        case h +: t =>
          val xs = OneAnd(h, t.toSet - h)
          if (xs.length != t.size + 1) throw new IllegalArgumentException(errorMsg)
          else xs
      }
      .reads(s)
  }

  private implicit val partyRead: Read[Party] = Read.reads { (s: String) =>
    Party(s)
  }

  def parseConfig(args: Seq[String]): Option[Config] =
    configParser.parse(args, Config.Empty)

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private val configParser: scopt.OptionParser[Config] =
    new scopt.OptionParser[Config]("transaction-dump-binary") {

      override def renderingMode: RenderingMode = RenderingMode.OneColumn

      head("Transaction Dump")

      help("help").text("Print this usage text")

      opt[String]("ledger-host")
        .action((x, c) => c.copy(ledgerHost = x))
        .required()
        .text("Ledger host name or IP address")

      opt[Int]("ledger-port")
        .action((x, c) => c.copy(ledgerPort = x))
        .required()
        .text("Ledger port number")

      opt[OneAnd[Set, Party]]("parties")
        .action((x, c) => c.copy(parties = x))
        .required()
        .text("Set of parties")

      opt[Duration]("message-rate-interval")
        .action((x, c) => c.copy(messageRateInterval = FiniteDuration(x.length, x.unit)))
        .required()
        .text("Time interval for message rate calculation")
    }
}
