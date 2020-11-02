// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.nonrepudiation

import java.io.ByteArrayInputStream
import java.security.{PublicKey, Signature}

import com.google.common.io.BaseEncoding
import io.grpc.ForwardingServerCall.SimpleForwardingServerCall
import io.grpc.ForwardingServerCallListener.SimpleForwardingServerCallListener
import io.grpc.Metadata.Key
import io.grpc._

import scala.util.{Failure, Success, Try}

class SigningInterceptor(publicKey: PublicKey) extends ServerInterceptor {
  val sigKey: Key[String] = Key.of("signature", Metadata.ASCII_STRING_MARSHALLER)
  def verify(msg: Array[Byte], msgSig: Array[Byte]): Boolean = {
    val sig = Signature.getInstance("SHA256withRSA")
    sig.initVerify(publicKey)
    sig.update(msg)
    sig.verify(msgSig)
  }
  override def interceptCall[ReqT, RespT](
      call: ServerCall[ReqT, RespT],
      headers: Metadata,
      next: ServerCallHandler[ReqT, RespT]): ServerCall.Listener[ReqT] = {
    val forwarded = new SimpleForwardingServerCall[ReqT, RespT](call) {
      override def sendMessage(message: RespT): Unit = {
        // val body = message.asInstanceOf[Array[Byte]];
        super.sendMessage(message)
      }
    }
    val listener = next.startCall(forwarded, headers)
    new SimpleForwardingServerCallListener[ReqT](listener) {
      override def onMessage(message: ReqT): Unit = {
        val bytes = message.asInstanceOf[Array[Byte]]
        val expectedHash: String = headers.get(sigKey)
        val sigOk = Try(verify(bytes, BaseEncoding.base64().decode(expectedHash)))
        sigOk match {
          case Failure(ex) => call.close(Status.UNAUTHENTICATED.withDescription(ex.toString), new Metadata())
          case Success(b) => if (b) {
            val input = new ByteArrayInputStream(bytes)
            val dup = call.getMethodDescriptor.parseRequest(input)
            super.onMessage(dup)
        } else {
            call.close(Status.UNAUTHENTICATED.withDescription("Signature verification failed"), new Metadata())
        }
        }
      }
    }
  }
}
