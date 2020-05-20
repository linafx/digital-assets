// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.transaction.dump

import java.nio.file.{Path, Paths}

import com.daml.ledger.api.refinements.ApiTypes.Party
import scalaz.{OneAnd, Show, Tag}
import scopt.{Read, RenderingMode}
import scalaz.std.string._

import scala.concurrent.duration.{Duration, FiniteDuration}

// TODO(Leo): consider merging it with com.daml.http.Config or factoring out the common parts
final case class Config(
    ledgerHost: String,
    ledgerPort: Int,
    parties: OneAnd[Set, Party],
    reportInterval: FiniteDuration,
    accessTokenFile: Option[Path] = None,
)

object Config {
  val Empty = Config(
    ledgerHost = "",
    ledgerPort = -1,
    parties = OneAnd(Party(""), Set.empty),
    reportInterval = FiniteDuration(0L, java.util.concurrent.TimeUnit.NANOSECONDS))

  private implicit val partyShow: Show[Party] = Tag.subst(implicitly[Show[String]])

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val configShow: Show[Config] = Show.shows { c =>
    import scalaz.syntax.show._
    import scalaz.std.set._

    s"Config(ledgerHost=${c.ledgerHost}, ledgerPort=${c.ledgerPort}, parties=${c.parties.shows}, " +
      s"reportInterval=${c.reportInterval}, accessTokenFile=${c.accessTokenFile})"
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private implicit def nonEmptySetRead[A: Read]: Read[OneAnd[Set, A]] = Read.reads { s: String =>
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

  private implicit val partyRead: Read[Party] = Tag.subst(implicitly[Read[String]])

  private implicit val pathRead: Read[Path] = Read.reads { s: String =>
    val p = Paths.get(s)
    if (!p.toFile.canRead) throw new IllegalArgumentException(s"Cannot read from file: $p")
    else p
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
        .text("Sequence of unique party identifiers")

      opt[Duration]("report-interval")
        .action((x, c) => c.copy(reportInterval = FiniteDuration(x.length, x.unit)))
        .required()
        .text("Time interval specifying how often to report message rate statistics")

      opt[Path]("access-token-file")
        .action((x, c) => c.copy(accessTokenFile = Some(x)))
        .optional()
        .text(
          s"Provides the path from which the access token will be read, required to interact with an authenticated ledger")
    }
}
