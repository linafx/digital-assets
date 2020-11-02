// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.nonrepudiation

import com.daml.ledger.resources.ResourceContext
import com.daml.resources.ProgramResource

object Main {
  def main(args: Array[String]): Unit = {
    Config.parse(args) match {
      case None => sys.exit(1)
      case Some(config) =>
        new ProgramResource(new Runner(config)).run(ResourceContext.apply)
    }
  }
}
