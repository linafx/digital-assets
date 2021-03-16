// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep
//import scala.annotation.tailrec

object TreeRecursion {

  import com.daml.deep.Expression._

  def fibA(n: Long): Long = { evalA(makeFib(n)); }
  def leftA(n: Long): Long = { evalA(makeLeft(n)); }
  def rightA(n: Long): Long = { evalA(makeRight(n)); }

  // Initial example: Tree recursion. Recursive calls are *not* tail calls
  def evalA(e: Exp): Long = {
    e match {
      case Num(n) => n
      case Add(e1, e2) =>
        val v1 = evalA(e1)
        val v2 = evalA(e2)
        v1 + v2
    }
  }

  def fibB(n: Long): Long = { evalB(makeFib(n)); }
  def leftB(n: Long): Long = { evalB(makeLeft(n)); }
  def rightB(n: Long): Long = { evalB(makeRight(n)); }

  // Transformation 1 (CPS) : Every call is now a tail call. But still *not* self-tail-calls.
  // Transformation 2 (CPS+Trampoline) : Every call (via the trampoline) is a self-tail-call.
  def evalB(e0: Exp): Long = {
    ???
  }

}
