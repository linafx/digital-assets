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
  test("tripleA", K)

  // Mutual recursion...
  //test("offsetA", K)

  // Tree recursion...
  //test("fibA", 20)
  //test("leftA", K)
  //test("rightA", K)

}
