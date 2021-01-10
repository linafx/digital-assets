// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

import java.sql.{Connection, PreparedStatement}

import anorm.{BatchSql, NamedParameter}
import com.daml.platform.store.Conversions._
import com.daml.platform.store.DbType

private[events] sealed abstract class ContractWitnessesTable {

  protected val insert: String

  protected val TableName = "participant_contract_witnesses"
  protected val IdColumn = "contract_id"
  protected val WitnessColumn = "contract_witness"

  protected val delete = s"delete from $TableName where $IdColumn = {$IdColumn}"

  private def prepareBatchDelete(ids: Seq[ContractId]): Option[BatchSql] =
    batch(delete, ids.map(id => List[NamedParameter](IdColumn -> id)))

  def toExecutables(preparedRawEntries: Seq[PreparedRawEntry]): ContractWitnessesTable.Executables = {
    val witnessArchivals = preparedRawEntries.flatMap(_.contractWitnesses.netArchives).toSet

    ContractWitnessesTable.Executables(
      deleteWitnesses = prepareBatchDelete(witnessArchivals.toList),
      insertWitnesses = {
        (conn: Connection) => {
          val preparedStatement = conn.prepareStatement(insert)
          val netWitnesses: Seq[(ContractId, String)] = preparedRawEntries.flatMap(pre => Relation.flatten(pre.contractWitnesses.netVisibility))
            .filterNot{ case (coid, _) => witnessArchivals(coid)}
          val (witnessesContractIds, parties) = netWitnesses.map {
            case (id, party) => id.coid -> party
          }.unzip

          preparedStatement.setObject(1, witnessesContractIds.toArray)
          preparedStatement.setObject(2, parties.toArray)
          preparedStatement
        }
      }
    )
  }
}

private[events] object ContractWitnessesTable {

  final case class Executables(deleteWitnesses: Option[BatchSql], insertWitnesses: Connection => PreparedStatement)

  def apply(dbType: DbType): ContractWitnessesTable =
    dbType match {
      case DbType.Postgres => Postgresql
      case DbType.H2Database => H2Database
    }

  private object Postgresql extends ContractWitnessesTable {
    override protected val insert: String =
      s"""insert into $TableName($IdColumn, $WitnessColumn)
            select $IdColumn, $WitnessColumn
            from unnest(?, ?) as t($IdColumn, $WitnessColumn)
            on conflict do nothing"""
  }

  private object H2Database extends ContractWitnessesTable {
    override protected val insert: String =
      // BROKEN !!!
      s"merge into $TableName using dual on $IdColumn = {$IdColumn} and $WitnessColumn = {$WitnessColumn} when not matched then insert ($IdColumn, $WitnessColumn) values ({$IdColumn}, {$WitnessColumn})"
  }

}
