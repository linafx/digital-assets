// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.value.test

import com.daml.lf.transaction.TransactionVersion
import com.daml.lf.value.Value
import com.daml.lf.value.Value._

object ValueNormalizer {

  // equivalent to serialization + unserialization.
  def normalize(value0: Value[ContractId], version: TransactionVersion): Value[ContractId] = {

    import scala.Ordering.Implicits._

    val compress = (version >= TransactionVersion.minTypeErasure)

    def go(value: Value[ContractId]): Value[ContractId] =
      value match {
        case ValueEnum(id, cons, rank) =>
          ValueEnum(id.filterNot(_ => compress), cons, rank.filter(_ => compress))
        case ValueRecord(id, fields) =>
          ValueRecord(
            id.filterNot(_ => compress),
            fields.map { case (field, value) => field.filterNot(_ => compress) -> go(value) },
          )
        case ValueVariant(id, variant, rank, value) =>
          ValueVariant(id.filterNot(_ => compress), variant, rank.filter(_ => compress), go(value))
        case _: ValueCidlessLeaf | _: ValueContractId[_] => value
        case ValueList(values) =>
          ValueList(values.map(go))
        case ValueOptional(value) =>
          ValueOptional(value.map(go))
        case ValueTextMap(value) =>
          ValueTextMap(value.mapValue(go))
        case ValueGenMap(entries) =>
          ValueGenMap(entries.map { case (k, v) => go(k) -> go(v) })
      }

    go(value0)

  }

}
