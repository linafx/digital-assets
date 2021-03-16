// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep

object X {

  // Hey, I want my trampolines to form a Monad...

  import com.daml.deep.Expression._

  def fibC(n: Long): Long = { evalC(makeFib(n)); }
  def leftC(n: Long): Long = { evalC(makeLeft(n)); }
  def rightC(n: Long): Long = { evalC(makeRight(n)); }

  // Given these more powerful trampolines, we can avoid explicit passing/calling a
  // continuation parameter. Also, we can use for-comprehension syntax if we like.

  def evalC(e: Exp): Long = {

    def eval(e: Exp): Trampoline[Long] = {
      Bounce { () =>
        e match {
          case Num(num) =>
            Land(num)
          case Add(e1, e2) =>
            for {
              n1 <- eval(e1)
              n2 <- eval(e2)
            } yield (n1 + n2)
        }
      }
    }
    eval(e).run
  }

  // Definition for Trampolines which support monadic bind (flatMap)

  sealed trait Trampoline[A] {
    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
      FlatMap(this, f)
    }
    def map[B](f: A => B): Trampoline[B] = {
      flatMap { a =>
        Land(f(a))
      }
    }
    def run(): A = runViaSimp(this)
  }

  final case class Land[A](a: A) extends Trampoline[A]
  final case class Bounce[A](bounce: () => Trampoline[A]) extends Trampoline[A]
  final case class FlatMap[A, B](left: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  // The downside of Monadic Trampolines is they are more tricky to run with stack-safety.

  // One approach is to re-associate the Binds whilst running. see:
  //   https://medium.com/@olxc/trampolining-and-stack-safety-in-scala-d8e86474ddfa
  //   http://blog.higher-order.com/assets/trampolines.pdf

  // An alternative (equivalent?) approach is to linearize a monadic-trampoline to a
  // simple-trampoline, and then run the simple-trampoline. The linearizer is written in
  // CPS-style.

  import com.daml.deep.{Trampoline => simp}

  def runViaSimp[A](me: Trampoline[A]): A = {
    linearize(me) { a => simp.Land(a) }.run
  }

  def linearize[A, R](tr: Trampoline[A])(k: A => simp.Trampoline[R]): simp.Trampoline[R] = {
    simp.Bounce { _ =>
      tr match {
        case Land(a) =>
          k(a)
        case Bounce(bounce) =>
          linearize(bounce())(k)
        case FlatMap(left, f) =>
          linearize(left) { a =>
            linearize(f(a)) { b =>
              k(b)
            }
          }
      }
    }
  }

}
