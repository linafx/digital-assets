// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

import java.sql.Timestamp

import anorm.SqlQuery
import com.daml.ledger.participant.state.v1.DivulgedContract
import com.daml.platform.store.Conversions._
import com.daml.platform.store.Conversions.IntToSmallIntConversions._

object ContractsTablePostgres extends ContractsTable {

  private object Params {
    val contractIds = "contractIds"
    val templateIds = "templateIds"
    val createArgs = "createArgs"
    val timestamps = "timestamps"
    val hashes = "hashes"
    val stakeholders = "stakeholders"
    val createArgCompression = "createArgumentCompression"
  }

  private val insertContractQuery: SqlQuery = {
    import Params._
    anorm.SQL(
      s"""insert into participant_contracts(
       contract_id, template_id, create_argument, create_argument_compression, create_ledger_effective_time, create_key_hash, create_stakeholders
     )
     select
       contract_id, template_id, create_argument, create_argument_compression, create_ledger_effective_time, create_key_hash, string_to_array(create_stakeholders,'|')
     from
       unnest({$contractIds}, {$templateIds}, {$createArgs}, {$createArgCompression}, {$timestamps}, {$hashes}, {$stakeholders})
       as t(contract_id, template_id, create_argument, create_argument_compression, create_ledger_effective_time, create_key_hash, create_stakeholders)
            on conflict do nothing;""")
  }

  override def toExecutables(preparedRawEntries: Seq[PreparedRawEntry]): ContractsTable.Executables = {
    val allArchives = preparedRawEntries.flatMap(_.contracts.netArchives).toSet
    val allTimestampedCreates = for {
      rawEntry <- preparedRawEntries
      timestamp = java.sql.Timestamp.from(rawEntry.tx.ledgerEffectiveTime)
      create <- rawEntry.contracts.netCreates
    } yield create -> timestamp

    val transientContracts = allTimestampedCreates.filter { case (create, _) => allArchives.contains(create.coid) }
    val netCreates = allTimestampedCreates diff transientContracts
    val netDeletes = allArchives diff transientContracts.map(_._1.coid).toSet

    val netDivulged = for {
      rawEntry <- preparedRawEntries
      divulgedContract <- rawEntry.contracts.divulgedContracts
      if !allArchives(divulgedContract.contractId)
    } yield divulgedContract

    ContractsTable.Executables(
      deleteContracts = batch(deleteContractQuery, netDeletes.iterator.map(deleteContract).toVector),
      insertContracts = {
        val netCreatesSize = netCreates.size
        val divulgedSize = netDivulged.size
        val batchSize = netCreatesSize + divulgedSize

        val timestamps = netCreates.map(_._2).toArray ++ Array.fill[Timestamp](divulgedSize)(null)

        val contractIds, templateIds, stakeholders = Array.ofDim[String](batchSize)
        val createArgs, hashes = Array.ofDim[Array[Byte]](batchSize)

        val argsByContractId = preparedRawEntries.flatMap { pre =>
          netCreates.map { case (c, _) => c.coid.coid -> pre.compressed.contracts } ++
            netDivulged.map(dc => dc.contractId.coid -> pre.compressed.contracts)
        }.toMap

        netCreates.iterator.zipWithIndex.foreach { case ((create, _), idx) =>
          contractIds(idx) = create.coid.coid
          templateIds(idx) = create.templateId.toString
          stakeholders(idx) = create.stakeholders.mkString("|")
          createArgs(idx) = argsByContractId(create.coid.coid).createArguments(create.coid)
          hashes(idx) = create.versionedKey.map(convert(create.templateId, _)).map(_.hash.bytes.toByteArray).orNull
        }

        netDivulged.iterator.zipWithIndex.foreach {
          case (DivulgedContract(contractId, contractInst), idx) =>
            contractIds(idx + netCreatesSize) = contractId.coid
            templateIds(idx + netCreatesSize) = contractInst.template.toString
            stakeholders(idx + netCreatesSize) = ""
            createArgs(idx + netCreatesSize) = argsByContractId(contractId.coid).createArguments(contractId)
            hashes(idx + netCreatesSize) = null
        }

        val createArgCompressions = contractIds.map(argsByContractId).map(_.createArgumentsCompression.id)

        if (batchSize == 0) None else Some(insertContractQuery.on(
          Params.contractIds -> contractIds,
          Params.templateIds -> templateIds,
          Params.createArgs -> createArgs,
          Params.timestamps -> timestamps,
          Params.hashes -> hashes,
          Params.stakeholders -> stakeholders,
          Params.createArgCompression -> createArgCompressions,
        )
        )
      }
    )
  }
}
