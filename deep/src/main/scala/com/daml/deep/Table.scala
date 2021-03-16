// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.deep

import com.daml.deep.{SimpleRecursion => SR}
import com.daml.deep.{MutualRecursion => MR}
import com.daml.deep.{TreeRecursion => TR}

object Table {

  def table = Map[String, Long => Long](
    // Simple recursion
    ("tripleA", SR.tripleA),
    ("tripleB", SR.tripleB),
    // Mutual recursion...
    ("offsetA", MR.offsetA),
    ("offsetB", MR.offsetB),
    // Tree recursion...
    ("fibA", TR.fibA),
    ("leftA", TR.leftA),
    ("rightA", TR.rightA),
    ("fibB", TR.fibB),
    ("leftB", TR.leftB),
    ("rightB", TR.rightB),
    ("fibC", X.fibC),
    ("leftC", X.leftC),
    ("rightC", X.rightC),
  )

}
