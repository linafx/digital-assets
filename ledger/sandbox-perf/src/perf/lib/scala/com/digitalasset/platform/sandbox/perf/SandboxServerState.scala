// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox.perf

import java.io.File

import com.digitalasset.platform.sandbox.SandboxServer
import com.digitalasset.platform.sandbox.config.{DamlPackageContainer, SandboxConfig}

class SandboxServerState {

  private var _app: SandboxServer.SandboxServer = null

  def setup(): Unit = {
    println("Starting Sandbox Application")
    _app = SandboxServer(
      SandboxConfig.default
        .copy(port = 0, damlPackageContainer = DamlPackageContainer(List(new File("//TODO")))))
    _app.start()
  }

  def close(): Unit = {
    println("Stopping Sandbox Application")
    _app.close()
    _app = null
  }

  def app: SandboxServer.SandboxServer = _app

}
