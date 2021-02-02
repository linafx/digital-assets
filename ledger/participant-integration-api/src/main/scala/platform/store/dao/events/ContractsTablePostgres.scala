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

  override def toExecutables(preparedRawEntries: Seq[PreparedRawEntry]): ContractsTable.Executables =
    ContractsTable.Executables(
      deleteContracts = batch(deleteContractQuery, preparedRawEntries.flatMap {
        case PreparedRawEntry(_, _, _, contracts, _) =>
          contracts.netArchives.iterator.map(deleteContract)
      }),
      insertContracts = {
        val allNetArchives = preparedRawEntries.flatMap(_.contracts.netArchives).toSet

        val allNetCreates =
          preparedRawEntries.flatMap {
            pre =>
              pre.contracts.netCreates.map(create => create -> java.sql.Timestamp.from(pre.tx.ledgerEffectiveTime))
          }.filterNot(create => allNetArchives(create._1.coid))
        val allDivulged = preparedRawEntries.flatMap {
          pre =>
            pre.contracts.divulgedContracts.map {
              dc => dc -> java.sql.Timestamp.from(pre.tx.ledgerEffectiveTime)
            }
        }.filterNot(dc => allNetArchives(dc._1.contractId))

        val netCreatesSize = allNetCreates.size
        val divulgedSize = allDivulged.size
        val batchSize = netCreatesSize + divulgedSize

        val timestamps = allNetCreates.map(_._2).toArray ++ Array.fill[Timestamp](divulgedSize)(null)

        val contractIds, templateIds, stakeholders = Array.ofDim[String](batchSize)
        val createArgs, hashes = Array.ofDim[Array[Byte]](batchSize)

        val argsBySerialized = preparedRawEntries.flatMap { pre =>
          pre.contracts.netCreates.map(c => c.coid -> pre.compressed.contracts) ++
            pre.contracts.divulgedContracts.map(dc => dc.contractId -> pre.compressed.contracts)
        }.toMap

        allNetCreates.iterator.zipWithIndex.foreach { case ((create, _), idx) =>
          contractIds(idx) = create.coid.coid
          templateIds(idx) = create.templateId.toString
          stakeholders(idx) = create.stakeholders.mkString("|")
          createArgs(idx) = argsBySerialized(create.coid).createArguments(create.coid)
          hashes(idx) = create.versionedKey.map(convert(create.templateId, _)).map(_.hash.bytes.toByteArray).orNull
        }

        allDivulged.iterator.zipWithIndex.foreach {
          case ((DivulgedContract(contractId, contractInst), _), idx) =>
            contractIds(idx + netCreatesSize) = contractId.coid
            templateIds(idx + netCreatesSize) = contractInst.template.toString
            stakeholders(idx + netCreatesSize) = ""
            createArgs(idx + netCreatesSize) = argsBySerialized(contractId).createArguments(contractId)
            hashes(idx + netCreatesSize) = null
        }

        val createArgCompressions =
          Array.fill[Option[Int]](batchSize)(argsBySerialized.headOption.flatMap(_._2.createArgumentsCompression.id)) // NOT CORRECT

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
