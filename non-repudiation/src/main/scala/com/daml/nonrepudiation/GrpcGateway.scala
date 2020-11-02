// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.nonrepudiation

import java.io.{ByteArrayInputStream, InputStream}

import com.google.common.io.ByteStreams
import io.grpc.MethodDescriptor.MethodType
import io.grpc.stub.ServerCalls.{
  BidiStreamingMethod,
  ClientStreamingMethod,
  ServerStreamingMethod,
  UnaryMethod
}
import io.grpc.stub.{ClientCalls, ServerCalls, StreamObserver}
import io.grpc._

import scala.collection.mutable.ArrayBuffer

object GrpcGateway {
  def buildServiceGateway(
      protoDescriptor: ServiceDescriptor,
      delegateServer: Channel,
      callOptions: CallOptions): ServerServiceDefinition = {
    // We need a custom descriptor for reflection to work.
    val builder = ServiceDescriptor.newBuilder(protoDescriptor.getName)
    builder.setSchemaDescriptor(protoDescriptor.getSchemaDescriptor)

    val methods = new ArrayBuffer[
      (MethodDescriptor[Array[Byte], Array[Byte]], ServerCallHandler[Array[Byte], Array[Byte]])]
    protoDescriptor.getMethods.forEach(m => {
      val descBuilder = MethodDescriptor.newBuilder(ByteMarshaller, ByteMarshaller)
      descBuilder.setType(m.getType)
      descBuilder.setFullMethodName(m.getFullMethodName)
      val desc = descBuilder.build
      builder.addMethod(desc)
      methods += (
        (
          desc,
          createProxyMethodDefinition(descBuilder.build, delegateServer, callOptions)))
    })
    val serviceBuilder = ServerServiceDefinition.builder(builder.build)
    methods.toArray.iterator.foreach {
      case (desc, call) => serviceBuilder.addMethod(desc, call)
    }
    serviceBuilder.build
  }

  private def createProxyMethodDefinition(
      method: MethodDescriptor[Array[Byte], Array[Byte]],
      delegateServerChannel: Channel,
      callOptions: CallOptions
  ): ServerCallHandler[Array[Byte], Array[Byte]] = {
    val newClientCall = () => delegateServerChannel.newCall(method, callOptions)

    method.getType match {
      case MethodType.UNARY =>
        ServerCalls.asyncUnaryCall(new UnaryMethod[Array[Byte], Array[Byte]] {
          override def invoke(
              request: Array[Byte],
              responseObserver: StreamObserver[Array[Byte]]): Unit =
            ClientCalls.asyncUnaryCall(newClientCall(), request, responseObserver)
        })
      case MethodType.CLIENT_STREAMING =>
        ServerCalls.asyncClientStreamingCall(new ClientStreamingMethod[Array[Byte], Array[Byte]] {
          override def invoke(
              responseObserver: StreamObserver[Array[Byte]]): StreamObserver[Array[Byte]] =
            ClientCalls.asyncClientStreamingCall(newClientCall(), responseObserver)
        })
      case MethodType.SERVER_STREAMING =>
        ServerCalls.asyncServerStreamingCall(new ServerStreamingMethod[Array[Byte], Array[Byte]] {
          override def invoke(
              request: Array[Byte],
              responseObserver: StreamObserver[Array[Byte]]): Unit =
            ClientCalls.asyncServerStreamingCall(newClientCall(), request, responseObserver)
        })
      case MethodType.BIDI_STREAMING =>
        ServerCalls.asyncBidiStreamingCall(new BidiStreamingMethod[Array[Byte], Array[Byte]] {
          override def invoke(
              responseObserver: StreamObserver[Array[Byte]]): StreamObserver[Array[Byte]] =
            ClientCalls.asyncBidiStreamingCall(newClientCall(), responseObserver)
        })
      case MethodType.UNKNOWN =>
        throw new GrpcGatewayException(s"${method.getFullMethodName} has unknown type")
    }
  }

  private object ByteMarshaller extends MethodDescriptor.Marshaller[Array[Byte]] {
    def parse(value: InputStream): Array[Byte] = ByteStreams.toByteArray(value)
    def stream(value: Array[Byte]) = new ByteArrayInputStream(value)
  }
}

class GrpcGatewayException(message: String) extends Exception(message)
