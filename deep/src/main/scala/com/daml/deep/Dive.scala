// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep

object Dive extends App {

  import com.daml.deep.Table.table

  def test(name: String, arg: Long) = {
    val fun = table(name)
    val res = fun(arg)
    println(s"$name ($arg) -> $res")
  }

  val H = 100L
  val K = 1000L
  val M = K * K
  val G = M * K

  // Simple recursion...
  test("tripleB", M)

  // Mutual recursion...
  test("offsetB", M)

  // Tree recursion...
  test("fibB", 20)
  test("leftB", M)
  test("rightB", M)

}
