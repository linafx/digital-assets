// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

object ContractWitnessesTableH2 extends ContractWitnessesTable {
  override def toExecutables(preparedRawEntries: Seq[PreparedRawEntry]): ContractWitnessesTable.Executables = ???
}
