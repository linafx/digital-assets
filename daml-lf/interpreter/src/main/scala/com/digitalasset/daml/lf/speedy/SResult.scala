// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy

import com.daml.lf.CompiledPackages
import com.daml.lf.value.Value.{ContractId, ContractInst}
import com.daml.lf.data.Ref._
import com.daml.lf.data.Time
import com.daml.lf.transaction.{GlobalKeyWithMaintainers, SubmittedTransaction}
import com.daml.lf.speedy.SError._
import com.daml.lf.value.Value

/** The result from small-step evaluation.
  * If the result is not Done or Continue, then the machine
  * must be fed before it can be stepped further.
  */
sealed abstract class SResult extends Product with Serializable

object SResult {
  final case class SResultError(err: SError) extends SResult

  /** The speedy machine has completed evaluation to reach a final value. */
  final case class SResultFinalValue(v: SValue) extends SResult

  /** Update or scenario interpretation requires the current
    * ledger time.
    */
  final case class SResultNeedTime(callback: Time.Timestamp => Unit) extends SResult

  /** Update interpretation requires access to a contract on the ledger. */
  final case class SResultNeedContract(
      contractId: ContractId,
      templateId: TypeConName,
      committers: Set[Party],
      // Callback to signal that the contract was not present
      // or visible. Returns true if this was recoverable.
      cbMissing: Unit => Boolean,
      cbPresent: ContractInst[Value.VersionedValue[ContractId]] => Unit,
  ) extends SResult

  /** Machine needs a definition that was not present when the machine was
    * initialized. The caller must retrieve the definition and fill it in
    * the packages cache it had provided to initialize the machine.
    */
  final case class SResultNeedPackage(
      pkg: PackageId,
      callback: CompiledPackages => Unit,
  ) extends SResult

  /** Commit the partial transaction to the (scenario) ledger.
    * Machine expects the value back with the contract ids rewritten
    * to be absolute.
    */
  final case class SResultScenarioCommit(
      value: SValue,
      tx: SubmittedTransaction,
      committers: Set[Party],
      callback: SValue => Unit,
  ) extends SResult

  final case class SResultScenarioInsertMustFail(
      committers: Set[Party],
      optLocation: Option[Location],
  ) extends SResult

  /** A "must fail" update resulted in a partial transaction, try and
    * commit this transaction with the expectation that it fails.
    * The callback signals success and clears the partial transaction.
    */
  final case class SResultScenarioMustFail(
      ptx: SubmittedTransaction,
      committers: Set[Party],
      callback: Unit => Unit,
  ) extends SResult

  /** Pass the ledger time and return back the new ledger time. */
  final case class SResultScenarioPassTime(
      relTime: Long,
      callback: Time.Timestamp => Unit,
  ) extends SResult

  /** A conversion of a string into a party is requested. */
  final case class SResultScenarioGetParty(
      partyText: String,
      callback: Party => Unit,
  ) extends SResult

  final case class SResultNeedKey(
      key: GlobalKeyWithMaintainers,
      committers: Set[Party],
      // Callback.
      // returns true if machine can continue with the given result.
      cb: SKeyLookupResult => Boolean,
  ) extends SResult

  final case class SResultNeedLocalKeyVisible(
      stakeholders: Set[Party],
      committers: Set[Party],
      cb: SVisibleByKey => Unit,
  ) extends SResult

  sealed abstract class SVisibleByKey
  object SVisibleByKey {
    // actAs and readAs are only included for better error messages.
    final case class NotVisible(
        actAs: Set[Party],
        readAs: Set[Party],
    ) extends SVisibleByKey
    final case object Visible extends SVisibleByKey

    def fromSubmitters(
        actAs: Set[Party],
        readAs: Set[Party] = Set.empty,
    ): Set[Party] => SVisibleByKey = {
      val readers = actAs union readAs
      stakeholders =>
        if (readers.intersect(stakeholders).nonEmpty) {
          SVisibleByKey.Visible
        } else {
          SVisibleByKey.NotVisible(actAs, readAs)
        }
    }
  }

  sealed abstract class SKeyLookupResult
  object SKeyLookupResult {
    final case class Found(coid: ContractId) extends SKeyLookupResult
    final case object NotFound extends SKeyLookupResult
    final case object NotVisible extends SKeyLookupResult

    def apply(coid: Option[ContractId]): SKeyLookupResult =
      coid.fold[SKeyLookupResult](NotFound)(Found)
  }

}
