// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy

import scala.annotation.tailrec

private[lf] object ExploreTrampoline {

  // Small expression evaluation example.
  sealed trait Exp
  final case class Num(num: Int) extends Exp
  final case class Add(e1: Exp, e2: Exp) extends Exp

  // eval0 is the original stack-unsafe evaluator.
  def eval0(e: Exp): Int = {
    e match {
      case Num(num) =>
        num
      case Add(e1, e2) =>
        val n1 = eval0(e1)
        val n2 = eval0(e2)
        n1 + n2
    }
  }

  //----------------------------------------------------------------------
  // Approach 1 -- CPS+(simple)trampolines...

  sealed trait SimTramp[A]
  final case class SimLand[A](a: A) extends SimTramp[A]
  final case class SimBounce[A](bounce: () => SimTramp[A]) extends SimTramp[A]

  // Run a simple trampoline using a tail recursive loop
  @tailrec
  def runSimTramp[A](tr: SimTramp[A]): A = {
    tr match {
      case SimLand(a) => a
      case SimBounce(bounce) => runSimTramp(bounce())
    }
  }

  // Convert eval code to CPS. The type of the final return is polymorphic, but it must be
  // a Trampoline to allow the Bounce insertion, which is what makes the code stack-safe.

  def evalCpsSimTramp[R](e: Exp)(k: Int => SimTramp[R]): SimTramp[R] = {
    SimBounce { () =>
      e match {
        case Num(num) =>
          k(num)
        case Add(e1, e2) =>
          evalCpsSimTramp(e1) { n1 =>
            evalCpsSimTramp(e2) { n2 =>
              k(n1 + n2)
            }
          }
      }
    }
  }

  // eval1 calls evalCpsSimTramp, passing the final continuation to land the trampoline,
  // and specialising the final result as SimTramp[Int]. It then calls runSimTramp to
  // force the result to an Int.

  def eval1(e: Exp): Int = {
    runSimTramp(evalCpsSimTramp(e) { res =>
      SimLand(res)
    })
  }

  //----------------------------------------------------------------------
  // Approach 2 -- No CPS transform. Trampolines must support flatMap (i.e form a monad)

  sealed trait MonTramp[A] {
    def flatMap[B](f: A => MonTramp[B]): MonTramp[B] = {
      MonFlatMap(this, f)
    }
    def map[B](f: A => B): MonTramp[B] = {
      flatMap { a =>
        MonLand(f(a))
      }
    }
  }
  final case class MonLand[A](a: A) extends MonTramp[A]
  final case class MonBounce[A](bounce: () => MonTramp[A]) extends MonTramp[A]
  final case class MonFlatMap[A, B](left: MonTramp[A], f: A => MonTramp[B]) extends MonTramp[B]

  // Given these more powerful trampolines, we can avoid explicit passing/calling a
  // continuation parameter. Also, we can use for-comprehension syntax if we like.

  def evalMonTramp(e: Exp): MonTramp[Int] = {
    MonBounce { () =>
      e match {
        case Num(num) =>
          MonLand(num)
        case Add(e1, e2) =>
          for {
            n1 <- evalMonTramp(e1)
            n2 <- evalMonTramp(e2)
          } yield (n1 + n2)
      }
    }
  }

  def eval2(e: Exp): Int = {
    runMonTramp(evalMonTramp(e))
  }

  // The negative side comes here: It is more tricky to ensure stack-safety when running a
  // monadic trampoline. One approach is to re-associate the Binds during running.
  // https://medium.com/@olxc/trampolining-and-stack-safety-in-scala-d8e86474ddfa

  // An alternative (equivalent?) approach is to linearize the monadic-trampolines to
  // simple-trampolines, and run the simple-trampoline. The linearizer is written in
  // CPS-style.

  def runMonTramp[A](monTr: MonTramp[A]): A = {
    runSimTramp(linearize(monTr) { a => SimLand(a) })
  }

  def linearize[A, R](tr: MonTramp[A])(k: A => SimTramp[R]): SimTramp[R] = {
    SimBounce { () =>
      tr match {
        case MonLand(a) =>
          k(a)
        case MonBounce(bounce) =>
          linearize(bounce())(k)
        case MonFlatMap(left, f) =>
          linearize(left) { a =>
            linearize(f(a)) { b =>
              k(b)
            }
          }
      }
    }
  }

}
