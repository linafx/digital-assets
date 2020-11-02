// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.api.signing.client;

import com.google.common.hash.Hashing;
import com.google.common.hash.HashingInputStream;
import com.google.common.io.BaseEncoding;
import io.grpc.*;
import io.grpc.stub.AbstractStub;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.security.*;
import java.util.concurrent.Executor;
import com.google.common.io.ByteStreams;

public final class SigningInterceptor implements ClientInterceptor {

    private PrivateKey signingKey;

    private byte[] sign(byte[] input) {
        try {
            Signature sig = Signature.getInstance("SHA256withRSA");
            sig.initSign(signingKey);
            sig.update(input);
            return sig.sign();
        } catch (NoSuchAlgorithmException | InvalidKeyException | SignatureException e) {
            throw new RuntimeException(e);
        }
    }

  public static class ByteMarshaller implements MethodDescriptor.Marshaller<byte[]> {
    @Override
    public byte[] parse(InputStream value) {
        try {
            return ByteStreams.toByteArray(value);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
    @Override
      public InputStream stream(byte[] value) {
          return new ByteArrayInputStream(value);
      }
  }

    private static Metadata.Key<String> signatureKey =
            Metadata.Key.of("signature", Metadata.ASCII_STRING_MARSHALLER);

    public SigningInterceptor(PrivateKey signingKey) {
        super();
        this.signingKey = signingKey;
    }

    @Override
    public <ReqT, RespT> ClientCall<ReqT, RespT> interceptCall(MethodDescriptor<ReqT, RespT> method,
                                                               CallOptions callOptions, Channel next) {
      ClientCall<ReqT, RespT> call = next.newCall(method, callOptions);
      return new ForwardingClientCall.SimpleForwardingClientCall<ReqT, RespT>(call) {
          Listener<RespT> responseListener = null;
          Metadata headers = null;
          boolean started = false;
          int requested = 0;
          @Override public void start(Listener<RespT> responseListener, Metadata headers) {
            // Delay start until we have the message body since
            // the signature in the Metadata depends on the body.
            this.responseListener = responseListener;
            this.headers = headers;
          }
          @Override public void request(int numMessages) {
              // Delay until we have the message body since the
              // signature in the Metadata depends on the body.
              requested += numMessages;
          }
          @Override
          public void sendMessage(ReqT message) {
              byte[] is = new ByteMarshaller ().parse(method.getRequestMarshaller().stream(message));
              String hashBase64 = BaseEncoding.base64().encode(sign(is));
              headers.put(signatureKey, hashBase64);
              if (!started) {
                  delegate().start(responseListener, headers);
                  started = true;
              }
              // I have no idea if the order of `request` and `sendMessage` matters.
              delegate().request(requested);
              delegate().sendMessage(message);
          }
      };
    }
}
