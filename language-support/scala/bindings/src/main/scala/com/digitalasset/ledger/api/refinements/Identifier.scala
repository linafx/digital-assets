// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.ledger.api.refinements

import com.digitalasset.ledger.api.v1.value.{Identifier => RPCIdentifier}

final case class Identifier(packageId: String, moduleName: String, entityName: String) {
  def toRpc: RPCIdentifier
}

object Identifier extends ((String, String, String) => Identifier) {
  type RawRPC = RPCIdentifier

  private[this] val colonSeparated = raw"([^:]+):([^:]+)".r
  private[this] val dotSeparated = raw"(?s)(.+)\.(.+)".r

  @throws[IllegalArgumentException]
  def fromRawRPC(ri: RawRPC): Identifier =
    if (ri.moduleName.nonEmpty && ri.entityName.nonEmpty)
      apply(ri.packageId, ri.moduleName, ri.entityName)
    else
      ri.name match {
        case colonSeparated(mn, en) => apply(ri.packageId, mn, en)
        case dotSeparated(mn, en) => apply(ri.packageId, mn, en)
        case name =>
          throw new IllegalArgumentException(
            s"'$name' can't be parsed into a module and entity name")
      }

  @deprecated(
    "use fromRawRPC explicitly for conversion; remove the RPC Identifier from data models",
    since = "0.12.13")
  implicit def `Identifier fromRawRPC`(ri: RawRPC): Identifier =
    fromRawRPC(ri)

  object Legacy {
    def unapply(t: Identifier): Some[(String, String)] =
      Some((t.packageId, s"${t.moduleName}.${t.entityName}"))
  }
}
