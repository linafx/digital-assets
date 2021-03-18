// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep
import scala.annotation.tailrec

object SimpleRecursion {

  // Initial example: Not tail recursive.
  def tripleA(a: Long): Long = {
    if (a == 0) {
      0
    } else {
      tripleA(a - 1) + 3
    }
  }

  // Solution: Introduce accumulator to gain tail calls. More specifically, self-tail-calls.
  def tripleB(a0: Long): Long = {

    @tailrec
    def loop(a: Long, acc: Long): Long = {
      if (a == 0) {
        acc
      } else {
        loop(a - 1, acc + 3)
      }
    }
    loop(a0, 0)
  }

}
