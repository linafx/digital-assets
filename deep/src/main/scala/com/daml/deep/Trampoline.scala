// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep
import scala.annotation.tailrec

object Trampoline {

  sealed trait Trampoline[A] {
    @tailrec
    def run(): A = {
      this match {
        case Land(a) => a
        case Bounce(func) => func(()).run
      }
    }
  }

  final case class Land[A](a: A) extends Trampoline[A]
  final case class Bounce[A](func: Unit => Trampoline[A]) extends Trampoline[A]

}
