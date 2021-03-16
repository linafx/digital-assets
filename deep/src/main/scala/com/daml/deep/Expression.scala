// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep

object Expression {

  sealed trait Exp
  final case class Num(num: Long) extends Exp
  final case class Add(e1: Exp, e2: Exp) extends Exp

  def makeFib(n: Long): Exp = {
    if (n <= 1) {
      Num(n)
    } else {
      Add(makeFib(n - 1), makeFib(n - 2))
    }
  }

  def makeLeft(depth: Long): Exp = {
    var acc: Exp = Num(0)
    var i = 0
    while (i < depth) {
      i = i + 1
      acc = Add(acc, Num(7))
    }
    acc
  }

  def makeRight(depth: Long): Exp = {
    var acc: Exp = Num(0)
    var i = 0
    while (i < depth) {
      i = i + 1
      acc = Add(Num(8), acc)
    }
    acc
  }

}
