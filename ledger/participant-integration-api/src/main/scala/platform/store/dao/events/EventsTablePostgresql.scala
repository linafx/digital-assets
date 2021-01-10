// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

import java.lang
import java.sql.{Connection, PreparedStatement}
import java.time.Instant

import anorm.{BatchSql, NamedParameter}
import com.daml.ledger.participant.state.v1.{Offset, SubmitterInfo, TransactionId}
import com.daml.lf.ledger.EventId
import com.daml.platform.store.Conversions._

object EventsTablePostgresql extends EventsTable {

  /**
   * Insertions are represented by a single statement made of nested arrays, one per column, instead of JDBC batches.
   * This leverages a PostgreSQL-specific feature known as "array unnesting", which has shown to be considerable
   * faster than using JDBC batches.
   */
  final class Batches(
                       insertEvents: Connection => PreparedStatement,
                       updateArchives: Option[BatchSql],
                     ) extends EventsTable.Batches {
    override def executeEventsInsert()(implicit connection: Connection): Unit = {
      insertEvents(connection).executeBatch()
      updateArchives.foreach(_.execute())
    }
  }

  private val updateArchived =
    """update participant_events set create_consumed_at={consumed_at} where contract_id={contract_id} and create_argument is not null"""

  private def archive(consumedAt: Offset)(contractId: ContractId): Vector[NamedParameter] =
    Vector[NamedParameter](
      "consumed_at" -> consumedAt,
      "contract_id" -> contractId.coid,
    )

  override def toExecutables(preparedRawEntries: Seq[PreparedRawEntry]): EventsTable.Batches =
    new Batches(
      insertEvents = (conn: Connection) =>
        preparedRawEntries.foldLeft(conn.prepareStatement(batchInsertSqlString)){
          case (ps, PreparedRawEntry(tx, events, compressed, _, _)) =>
            prepareBatch(tx, events, compressed)(ps)
        },
      updateArchives = batch(updateArchived, preparedRawEntries.flatMap{
        case PreparedRawEntry(tx, events, _, _, _) =>
          events.archives.iterator.map(archive(tx.offset))
      })
    )

  private def prepareBatch(tx: TransactionIndexing.TransactionInfo, info: TransactionIndexing.EventsInfo, serialized: TransactionIndexing.Serialized)(preparedStatement: PreparedStatement): PreparedStatement = {
    val batchSize = info.events.size
    val eventIds = Array.ofDim[String](batchSize)
    val eventOffsets = Array.fill(batchSize)(tx.offset.toByteArray)
    val contractIds = Array.ofDim[String](batchSize)
    val transactionIds = Array.fill(batchSize)(tx.transactionId.asInstanceOf[String])
    val workflowIds = Array.fill(batchSize)(tx.workflowId.map(_.asInstanceOf[String]).orNull)
    val ledgerEffectiveTimes = Array.fill(batchSize)(tx.ledgerEffectiveTime)
    val templateIds = Array.ofDim[String](batchSize)
    val nodeIndexes = Array.ofDim[Integer](batchSize)
    val commandIds =
      Array.fill(batchSize)(tx.submitterInfo.map(_.commandId.asInstanceOf[String]).orNull)
    val applicationIds =
      Array.fill(batchSize)(tx.submitterInfo.map(_.applicationId.asInstanceOf[String]).orNull)
    val submitters = Array.ofDim[String](batchSize)
    val flatEventWitnesses = Array.ofDim[String](batchSize)
    val treeEventWitnesses = Array.ofDim[String](batchSize)
    val createArguments = Array.ofDim[Array[Byte]](batchSize)
    val createSignatories = Array.ofDim[String](batchSize)
    val createObservers = Array.ofDim[String](batchSize)
    val createAgreementTexts = Array.ofDim[String](batchSize)
    val createConsumedAt = Array.ofDim[Array[Byte]](batchSize)
    val createKeyValues = Array.ofDim[Array[Byte]](batchSize)
    val exerciseConsuming = Array.ofDim[lang.Boolean](batchSize)
    val exerciseChoices = Array.ofDim[String](batchSize)
    val exerciseArguments = Array.ofDim[Array[Byte]](batchSize)
    val exerciseResults = Array.ofDim[Array[Byte]](batchSize)
    val exerciseActors = Array.ofDim[String](batchSize)
    val exerciseChildEventIds = Array.ofDim[String](batchSize)

    val submittersValue = tx.submitterInfo.map(_.actAs.mkString("|")).orNull

    for (((nodeId, node), i) <- info.events.zipWithIndex) {
      node match {
        case create: Create =>
          submitters(i) = submittersValue
          contractIds(i) = create.coid.coid
          templateIds(i) = create.coinst.template.toString
          eventIds(i) = EventId(tx.transactionId, nodeId).toLedgerString
          nodeIndexes(i) = nodeId.index
          flatEventWitnesses(i) = info.stakeholders.getOrElse(nodeId, Set.empty).mkString("|")
          treeEventWitnesses(i) = info.disclosure.getOrElse(nodeId, Set.empty).mkString("|")
          createArguments(i) = serialized.createArguments(nodeId)
          createSignatories(i) = create.signatories.mkString("|")
          createObservers(i) = create.stakeholders.diff(create.signatories).mkString("|")
          if (create.coinst.agreementText.nonEmpty) {
            createAgreementTexts(i) = create.coinst.agreementText
          }
          createKeyValues(i) = serialized.createKeyValues.get(nodeId).orNull
        case exercise: Exercise =>
          submitters(i) = submittersValue
          contractIds(i) = exercise.targetCoid.coid
          templateIds(i) = exercise.templateId.toString
          eventIds(i) = EventId(tx.transactionId, nodeId).toLedgerString
          nodeIndexes(i) = nodeId.index
          flatEventWitnesses(i) = info.stakeholders.getOrElse(nodeId, Set.empty).mkString("|")
          treeEventWitnesses(i) = info.disclosure.getOrElse(nodeId, Set.empty).mkString("|")
          exerciseConsuming(i) = exercise.consuming
          exerciseChoices(i) = exercise.choiceId
          exerciseArguments(i) = serialized.exerciseArguments(nodeId)
          exerciseResults(i) = serialized.exerciseResults.get(nodeId).orNull
          exerciseActors(i) = exercise.actingParties.mkString("|")
          exerciseChildEventIds(i) = exercise.children
            .map(EventId(tx.transactionId, _).toLedgerString)
            .iterator
            .mkString("|")
        case _ => throw new UnexpectedNodeException(nodeId, tx.transactionId)
      }
    }

    preparedStatement.setObject(1, eventIds)
    preparedStatement.setObject(2, eventOffsets)
    preparedStatement.setObject(3, contractIds)
    preparedStatement.setObject(4, transactionIds)
    preparedStatement.setObject(5, workflowIds)
    preparedStatement.setArray(6, preparedStatement.getConnection.createArrayOf("TIMESTAMP", ledgerEffectiveTimes.map(java.sql.Timestamp.from))) // Handle instant
    preparedStatement.setObject(7, templateIds)
    preparedStatement.setObject(8, nodeIndexes)
    preparedStatement.setObject(9, commandIds)
    preparedStatement.setObject(10, applicationIds)
    preparedStatement.setObject(11, submitters)
    preparedStatement.setObject(12, flatEventWitnesses)
    preparedStatement.setObject(13, treeEventWitnesses)
    preparedStatement.setObject(14, createArguments)
    preparedStatement.setObject(15, createSignatories)
    preparedStatement.setObject(16, createObservers)
    preparedStatement.setObject(17, createAgreementTexts)
    preparedStatement.setObject(18, createConsumedAt)
    preparedStatement.setObject(19, createKeyValues)
    preparedStatement.setObject(20, exerciseConsuming)
    preparedStatement.setObject(21, exerciseChoices)
    preparedStatement.setObject(22, exerciseArguments)
    preparedStatement.setObject(23, exerciseResults)
    preparedStatement.setObject(24, exerciseActors)
    preparedStatement.setObject(25, exerciseChildEventIds)
    preparedStatement.addBatch()
    preparedStatement
  }

  private val batchInsertSqlString =
    """
       insert into participant_events(
           event_id, event_offset, contract_id, transaction_id, workflow_id, ledger_effective_time, template_id, node_index, command_id, application_id, submitters, flat_event_witnesses, tree_event_witnesses,
           create_argument, create_signatories, create_observers, create_agreement_text, create_consumed_at, create_key_value,
           exercise_consuming, exercise_choice, exercise_argument, exercise_result, exercise_actors, exercise_child_event_ids
         )
         select
           event_id, event_offset, contract_id, transaction_id, workflow_id, ledger_effective_time, template_id, node_index, command_id, application_id, string_to_array(submitters,'|'), string_to_array(flat_event_witnesses, '|'), string_to_array(tree_event_witnesses, '|'),
           create_argument, string_to_array(create_signatories,'|'), string_to_array(create_observers,'|'), create_agreement_text, create_consumed_at, create_key_value,
           exercise_consuming, exercise_choice, exercise_argument, exercise_result, string_to_array(exercise_actors,'|'), string_to_array(exercise_child_event_ids,'|')
         from
           unnest(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
           as
               t(
                 event_id, event_offset, contract_id, transaction_id, workflow_id, ledger_effective_time, template_id, node_index, command_id, application_id, submitters, flat_event_witnesses, tree_event_witnesses,
                 create_argument, create_signatories, create_observers, create_agreement_text, create_consumed_at, create_key_value,
                 exercise_consuming, exercise_choice, exercise_argument, exercise_result, exercise_actors, exercise_child_event_ids
               ) on conflict do nothing;
       """
}
