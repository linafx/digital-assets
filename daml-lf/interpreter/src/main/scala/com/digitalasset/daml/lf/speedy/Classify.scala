// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package speedy

import com.daml.lf.speedy.Speedy._
import com.daml.lf.speedy.SExpr._

object Classify { // classify the machine state w.r.t what step occurs next

  case class Counts(
      var steps: Int,

      // expression classification (ctrlExpr)
      var evalue: Int,
      var evar: Int,
      var eapp: Int,
      var eclose: Int,
      var ebuiltin: Int,
      var eval: Int,
      var elocation: Int,
      var elet: Int,
      var ecase: Int,
      var ebuiltinrecursivedefinition: Int,
      var ecatch: Int,
      var eimportvalue: Int,
      var ewronglytypedcontractid: Int,

      var efinished: Int,
      var eargs: Int,
      var efun: Int,
      var epap: Int,
      var ematch: Int,
      var ecacheval: Int,
      var epop: Int,
      var elocationtrace: Int,
      var ecatchmarker: Int,
      var ecollectarg: Int,

  ) {
    def pp: String = {
      List(
        ("- evalue", evalue),
        ("- evar", evar),
        ("- eapp", eapp),
        ("- eclose", eclose),
        ("- ebuiltin", ebuiltin),
        ("- eval", eval),
        ("- elocation", elocation),
        ("- elet", elet),
        ("- ecase", ecase),
        ("- ebuiltinrecursivedefinition", ebuiltinrecursivedefinition),
        ("- ecatch", ecatch),
        ("- eimportvalue", eimportvalue),

        ("- efun", efun),
        ("- epap", epap),
        ("- eargs", eargs),
        ("- efinished", efinished),
        ("- ematch", ematch),
        ("- ecacheval", ecacheval),
        ("- epop", epop),
        ("- ecatchmarker", ecatchmarker),
        ("- elocationtrace", elocationtrace),
        ("- ecollectarg", ecollectarg),

      ).map { case (tag, n) => s"$tag : $n" }.mkString("\n")
    }
  }

  def newEmptyCounts(): Counts = {
    Counts(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  def classifyMachine(machine: Machine, counts: Counts): Unit = {
    // classify a machine by the control expression
    counts.steps += 1
    classifyExpr(machine.ctrl, counts)
  }

  def classifyExpr(exp: SExpr, counts: Counts): Unit = {
    exp match {
      case _:SEValue => counts.evalue += 1
      case _:SEVar => counts.evar += 1
      case _:SEApp => counts.eapp += 1
      case _:SEMakeClo => counts.eclose += 1
      case _:SEBuiltin => counts.ebuiltin += 1
      case _:SEVal => counts.eval += 1
      case _:SELocation => counts.elocation += 1
      case _:SELet => counts.elet += 1
      case _:SECase => counts.ecase += 1
      case _:SEBuiltinRecursiveDefinition => counts.ebuiltinrecursivedefinition += 1
      case _:SECatch => counts.ecatch += 1
      case _:SEAbs => //never expect these!
      case _:SEImportValue => counts.eimportvalue += 1
      case _:SEWronglyTypeContractId => counts.ewronglytypedcontractid += 1
      case _:SEArgs => counts.eargs += 1
      case _:SEFun => counts.efun += 1
      case _:SEPAP => counts.epap += 1
      case _:SEFinished => counts.efinished += 1
      case _:SEMatch => counts.ematch += 1
      case _:SECacheVal => counts.ecacheval += 1
      case _:SEPop => counts.epop += 1
      case _:SELocationTrace => counts.elocationtrace += 1
      case _:SECatchMarker => counts.ecatchmarker += 1
      case _:SECollectArg => counts.ecollectarg += 1
    }
  }

}
