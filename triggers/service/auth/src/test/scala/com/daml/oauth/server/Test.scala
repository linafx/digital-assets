// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.oauth.server

import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.Uri.{Path, Query}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.daml.jwt.JwtDecoder
import com.daml.jwt.domain.Jwt
import com.daml.ledger.api.auth.{AuthServiceJWTCodec, AuthServiceJWTPayload}
import com.daml.ledger.api.testing.utils.SuiteResourceManagementAroundAll
import org.scalatest.{AsyncWordSpec, Matchers}
import spray.json._

import scala.concurrent.Future

class Test extends AsyncWordSpec with TestFixture with SuiteResourceManagementAroundAll with Matchers {
  import Client.JsonProtocol._
  private def requestAccess(parties: Seq[String], query: Query = Query()) = {
    lazy val clientBinding = suiteResource.value._2.localAddress
    lazy val clientUri = Uri().withAuthority(clientBinding.getHostString, clientBinding.getPort)
    val req = HttpRequest(
      uri = clientUri.withPath(Path./("access")).withScheme("http").withQuery(query),
      method = HttpMethods.POST,
      entity =
        HttpEntity(MediaTypes.`application/json`, Client.AccessParams(parties).toJson.compactPrint)
    )
    Http().singleRequest(req)
  }
  private def requestToken(parties: Seq[String]): Future[AuthServiceJWTPayload] = {
    for {
      resp <- requestAccess(parties)
      // Redirect to /authorize on authorization server (No automatic redirect handling in akka-http)
      resp <- {
        assert(resp.status == StatusCodes.Found)
        val req = HttpRequest(uri = resp.header[Location].get.uri)
        Http().singleRequest(req)
      }
      // Redirect to /cb on client.
      resp <- {
        assert(resp.status == StatusCodes.Found)
        val req = HttpRequest(uri = resp.header[Location].get.uri)
        Http().singleRequest(req)
      }
      // Actual token response (proxied from auth server to us via the client)
      body <- Unmarshal(resp).to[Client.AccessResponse]
      decodedJwt <- JwtDecoder
        .decode(Jwt(body.token))
        .fold((e => Future.failed(new IllegalArgumentException(e.toString))), Future.successful(_))
      payload <- Future.fromTry(AuthServiceJWTCodec.readFromString(decodedJwt.payload))
    } yield payload
  }

  "the auth server" should {
    "issue a token with no parties" in {
      for {
        token <- requestToken(Seq())
      } yield {
        assert(token.actAs == Seq())
      }
    }
    "issue a token with 1 party" in {
      for {
        token <- requestToken(Seq("Alice"))
      } yield {
        assert(token.actAs == Seq("Alice"))
      }
    }
    "issue a token with multiple parties" in {
      for {
        token <- requestToken(Seq("Alice", "Bob"))
      } yield {
        assert(token.actAs == Seq("Alice", "Bob"))
      }
    }
    "preserve a query string on the redirect URI" in {
      // The OAuth2 specification mandates that query parameters in the
      // redirect URI must be retained. This tests that the OAuth2 test server
      // follows that specification.
      val query = Query(("test", "parameter"))
      for {
        resp <- requestAccess(Seq(), query)
        // Redirect to /authorize on authorization server
        resp <- {
          assert(resp.status == StatusCodes.Found)
          val req = HttpRequest(uri = resp.header[Location].get.uri)
          Http().singleRequest(req)
        }
      } yield {
        assert(resp.status == StatusCodes.Found)
        resp.header[Location].get.uri.query().toList should contain allElementsOf (query.toList)
      }
    }
  }
}
