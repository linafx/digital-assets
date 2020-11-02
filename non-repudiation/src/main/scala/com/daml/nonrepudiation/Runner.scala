// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.nonrepudiation

import java.io.IOException
import java.net.{BindException, InetAddress, InetSocketAddress}
import java.nio.file.Files
import java.security.KeyFactory
import java.security.spec.X509EncodedKeySpec
import java.util.concurrent.TimeUnit.SECONDS

import akka.actor.ActorSystem
import com.daml.ledger.api.v1.admin.party_management_service.PartyManagementServiceGrpc
import com.daml.ledger.api.v1.command_service.CommandServiceGrpc
import com.daml.ledger.api.v1.ledger_identity_service.LedgerIdentityServiceGrpc
import com.daml.ledger.client.GrpcChannel
import com.daml.ledger.client.configuration.{CommandClientConfiguration, LedgerClientConfiguration, LedgerIdRequirement}
import com.daml.ledger.resources.{Resource, ResourceContext, ResourceOwner}
import com.daml.logging.ContextualizedLogger
import com.daml.ports.Port
import com.daml.resources
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.{CallOptions, Server, ServerInterceptors}

import scala.concurrent.Future

class Runner(config: Config) extends ResourceOwner[Port] {
  import Runner._
  override def acquire()(
      implicit context: ResourceContext): resources.Resource[ResourceContext, Port] = {
    val keyBytes = Files.readAllBytes(config.signingPublicKeyFile)
    val spec = new X509EncodedKeySpec(keyBytes)
    val kf = KeyFactory.getInstance("RSA")
    val key = kf.generatePublic(spec)
    val owner = for {
      _ <- ResourceOwner.forActorSystem(() => ActorSystem("non-repudiation"))
      upstreamChannel <- new GrpcChannel.Owner(
        NettyChannelBuilder.forAddress(config.upstreamHost, config.upstreamPort),
        LedgerClientConfiguration(
          applicationId = "unused appid",
          ledgerIdRequirement = LedgerIdRequirement.none,
          commandClient = CommandClientConfiguration.default,
          sslContext = None,
        )
      )
      server <- new ResourceOwner[Server] {
        override def acquire()(
            implicit context: ResourceContext): resources.Resource[ResourceContext, Server] =
          Resource(Future {
            val host =
              config.address.map(InetAddress.getByName).getOrElse(InetAddress.getLoopbackAddress)
            val builder = NettyServerBuilder
              .forAddress(new InetSocketAddress(host, config.port.value))
            builder.permitKeepAliveTime(10, SECONDS)
            builder.permitKeepAliveWithoutCalls(true)
            builder.directExecutor()
            builder.addService(
              GrpcGateway.buildServiceGateway(
                PartyManagementServiceGrpc.SERVICE,
                upstreamChannel,
                CallOptions.DEFAULT
              ))
            builder.addService(
              ServerInterceptors.intercept(
                GrpcGateway.buildServiceGateway(
                  CommandServiceGrpc.SERVICE,
                  upstreamChannel,
                  CallOptions.DEFAULT
                ),
                new SigningInterceptor(key)))
            builder.addService(
              GrpcGateway.buildServiceGateway(
                LedgerIdentityServiceGrpc.SERVICE,
                upstreamChannel,
                CallOptions.DEFAULT
              ))
            builder.addService(ProtoReflectionService.newInstance())
            val server = builder.build
            try {
              server.start
            } catch {
              case e: IOException if e.getCause != null && e.getCause.isInstanceOf[BindException] =>
                throw new RuntimeException(
                  s"The API server was unable to bind to port ${config.port}. Terminate the process occupying the port, or choose a different one.",
                  e.getCause)
            }
            logger.withoutContext.info("Started non-repudiation middleware")
            server
          })(server => Future(server.shutdown().awaitTermination))

      }
    } yield Port(server.getPort)
    owner.acquire()
  }
}

object Runner {
  private val logger = ContextualizedLogger.get(classOf[Runner])
}
