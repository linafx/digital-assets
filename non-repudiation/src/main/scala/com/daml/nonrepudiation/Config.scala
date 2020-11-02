// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.nonrepudiation

import java.nio.file.{Path, Paths}

import com.daml.ports.Port
import scopt.OptionParser

case class Config(
    upstreamHost: String,
    upstreamPort: Int,
    address: Option[String],
    port: Port,
    signingPublicKeyFile: Path,
)

object Config {
  def parse(args: Array[String]): Option[Config] =
    parser.parse(args, Empty)

  private val Empty = Config(
    upstreamHost = "",
    upstreamPort = -1,
    address = None,
    port = Port(0),
    signingPublicKeyFile = null,
  )

  private val parser = new OptionParser[Config]("non-repudiation") {
    head("non-repudiation")

    opt[String]("upstream-host")
      .required()
      .action((x, c) => c.copy(upstreamHost = x))
      .text("Upstream Ledger API host to forward requests to")

    opt[Int]("upstream-port")
      .required()
      .action((x, c) => c.copy(upstreamPort = x))
      .text("Upstream Ledger API port to forward requests to")

    opt[String]("address")
      .optional()
      .action((x, c) => c.copy(address = Some(x)))
      .text("Service host. Defaults to binding on localhost.")

    opt[Int]("port")
      .required()
      .action((x, c) => c.copy(port = Port(x)))
      .text("Service port")

    opt[String]("signing-public-key")
      .required()
      .action((x, c) => c.copy(signingPublicKeyFile = Paths.get(x)))
      .text("Path to public key")
  }
}
