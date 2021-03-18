// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep
//import scala.annotation.tailrec

object MutualRecursion {

  // Initial example: Mutually recursive tail calls. But *not* self-tail-calls.
  def offsetA(x: Long): Long = {
    def isEven(x: Long): Boolean = {
      if (x == 0) {
        true
      } else {
        isOdd(x - 1)
      }
    }
    def isOdd(x: Long): Boolean = {
      if (x == 0) {
        false
      } else {
        isEven(x - 1)
      }
    }
    if (isEven(x)) 4 else 5
  }

  // Solution: Trampolines. Every call (made via the trampoline) is a self-tail-call.
  def offsetB(x: Long): Long = {

    import com.daml.deep.Trampoline._

    def isEven(x: Long): Trampoline[Boolean] = {
      if (x == 0) {
        Land(true)
      } else {
        Bounce { _ =>
          isOdd(x - 1)
        }
      }
    }
    def isOdd(x: Long): Trampoline[Boolean] = {
      if (x == 0) {
        Land(false)
      } else {
        isEven(x - 1)
      }
    }
    if (isEven(x).run) 4 else 5
  }

}
