// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

import anorm.SqlQuery

object ContractWitnessesTablePostgres extends ContractWitnessesTable {
  val witnessesContractIdsParam = "witnessesContractIds"
  val partiesParam = "parties"

  private val insertWitnessesQuery: SqlQuery =
    anorm.SQL(
      s"""insert into $TableName($IdColumn, $WitnessColumn)
         |            select $IdColumn, $WitnessColumn
         |            from unnest({$witnessesContractIdsParam}, {$partiesParam}) as t($IdColumn, $WitnessColumn)
         |            on conflict do nothing;""".stripMargin)

  override def toExecutables(preparedRawEntries: Seq[PreparedRawEntry]): ContractWitnessesTable.Executables = {
    val witnessArchivals = preparedRawEntries.flatMap(_.contractWitnesses.netArchives).toSet

    ContractWitnessesTable.Executables(
      deleteWitnesses = prepareBatchDelete(witnessArchivals.toList),
      insertWitnesses = {
        val netWitnesses: Seq[(ContractId, String)] = preparedRawEntries.flatMap(pre => Relation.flatten(pre.contractWitnesses.netVisibility))
          .filterNot { case (coid, _) => witnessArchivals(coid) }
        val (witnessesContractIds, parties) = netWitnesses.map {
          case (id, party) => id.coid -> party
        }.toArray.unzip

        insertWitnessesQuery.on(
          witnessesContractIdsParam -> witnessesContractIds,
          partiesParam -> parties,
        )
      }
    )
  }
}
