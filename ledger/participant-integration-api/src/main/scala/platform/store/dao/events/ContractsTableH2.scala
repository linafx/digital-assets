// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

object ContractsTableH2 extends ContractsTable {
  override def toExecutables(preparedRawEntries: Seq[PreparedRawEntry]): ContractsTable.Executables = ???
}
