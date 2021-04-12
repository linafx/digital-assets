// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.api.testtool.infrastructure

import java.util.regex.Pattern

import com.softwaremill.diffx._
import com.google.protobuf.empty.Empty
import com.google.protobuf.timestamp.Timestamp
import com.daml.grpc.{GrpcException, GrpcStatus}
import com.daml.ledger.api.v1
import io.grpc.Status
import io.grpc.health.v1.health.HealthCheckResponse

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.control.NonFatal

object Assertions extends DiffExtensions {
  def fail(message: String): Nothing =
    throw new AssertionError(message)

  def fail(message: String, cause: Throwable): Nothing =
    throw new AssertionError(message, cause)

  def assertLength[A, F[_] <: Seq[_]](context: String, length: Int, as: F[A]): F[A] = {
    assert(as.length == length, s"$context: expected $length item(s), got ${as.length}")
    as
  }

  def assertSingleton[A](context: String, as: Seq[A]): A =
    assertLength(context, 1, as).head

  def assertEquals[T: Diff](context: String, actual: T, expected: T): Unit = {
    val diff = Diff.compare(actual, expected)
    if (!diff.isIdentical)
      throw AssertionErrorWithPreformattedMessage(
        diff.show(),
        s"$context: two objects are supposed to be equal but they are not",
      )
  }

  // Specialize a few instances via semi-automatic derivation
  // instead of using com.softwaremill.diffx.generic.auto.
  // This speeds up compilation of TransactionServiceIT from ~100s to <5s.

  implicit val diffEmpty: Derived[Diff[Empty]] = Diff.derived[Empty]
  implicit val diffTimestamp: Derived[Diff[Timestamp]] = Diff.derived[Timestamp]
  implicit val diffEventEvent: Derived[Diff[v1.event.Event.Event]] =
    Diff.derived[v1.event.Event.Event]
  implicit val diffTraceContext: Derived[Diff[v1.trace_context.TraceContext]] =
    Diff.derived[v1.trace_context.TraceContext]
  implicit val diffIdentifier: Derived[Diff[v1.value.Identifier]] =
    Diff.derived[v1.value.Identifier]
  implicit val diffValueSum: Derived[Diff[v1.value.Value.Sum]] = Diff.derived[v1.value.Value.Sum]
  implicit val diffValue: Derived[Diff[v1.value.Value]] = Diff.derived[v1.value.Value]
  implicit val diffVariant: Derived[Diff[v1.value.Variant]] = Diff.derived[v1.value.Variant]
  implicit val diffRecordField: Derived[Diff[v1.value.RecordField]] =
    Diff.derived[v1.value.RecordField]
  implicit val diffRecord: Derived[Diff[v1.value.Record]] = Diff.derived[v1.value.Record]
  implicit val diffEnum: Derived[Diff[v1.value.Enum]] = Diff.derived[v1.value.Enum]
  implicit val diffGenMapEntry: Derived[Diff[v1.value.GenMap.Entry]] =
    Diff.derived[v1.value.GenMap.Entry]
  implicit val diffGenMap: Derived[Diff[v1.value.GenMap]] = Diff.derived[v1.value.GenMap]
  implicit val diffList: Derived[Diff[v1.value.List]] = Diff.derived[v1.value.List]
  implicit val diffTextMapEntry: Derived[Diff[v1.value.Map.Entry]] =
    Diff.derived[v1.value.Map.Entry]
  implicit val diffTextMap: Derived[Diff[v1.value.Map]] = Diff.derived[v1.value.Map]
  implicit val diffOptional: Derived[Diff[v1.value.Optional]] = Diff.derived[v1.value.Optional]
  implicit val diffUnit: Derived[Diff[v1.value.Value.Sum.Unit]] =
    Diff.derived[v1.value.Value.Sum.Unit]
  implicit val diffServingStatus: Derived[Diff[HealthCheckResponse.ServingStatus]] =
    Diff.derived[HealthCheckResponse.ServingStatus]
  implicit val diffEvent: Derived[Diff[v1.event.Event]] = Diff.derived[v1.event.Event]
  implicit val diffCreatedEvent: Derived[Diff[v1.event.CreatedEvent]] =
    Diff.derived[v1.event.CreatedEvent]
  implicit val diffExercisedEvent: Derived[Diff[v1.event.ExercisedEvent]] =
    Diff.derived[v1.event.ExercisedEvent]
  implicit val diffArchivdeEvent: Derived[Diff[v1.event.ArchivedEvent]] =
    Diff.derived[v1.event.ArchivedEvent]
  implicit val diffTreeEventKind: Derived[Diff[v1.transaction.TreeEvent.Kind]] =
    Diff.derived[v1.transaction.TreeEvent.Kind]
  implicit val diffTreeEvent: Derived[Diff[v1.transaction.TreeEvent]] =
    Diff.derived[v1.transaction.TreeEvent]
  implicit val diffTransaction: Derived[Diff[v1.transaction.Transaction]] =
    Diff.derived[v1.transaction.Transaction]
  implicit val diffTransactionTree: Derived[Diff[v1.transaction.TransactionTree]] =
    Diff.derived[v1.transaction.TransactionTree]

  /** Match the given exception against a status code and a regex for the expected message.
    *      Succeeds if the exception is a GrpcException with the expected code and
    *      the regex matches some part of the message or there is no message and the pattern is
    *      None.
    */
  def assertGrpcError(t: Throwable, expectedCode: Status.Code, optPattern: Option[Pattern]): Unit =
    (t, optPattern) match {
      case (GrpcException(GrpcStatus(`expectedCode`, Some(msg)), _), Some(pattern)) =>
        if (pattern.matcher(msg).find()) {
          ()
        } else {
          fail(s"Error message did not contain [$pattern], but was [$msg].")
        }
      // None both represents pattern that we do not care about as well as
      // exceptions that have no message.
      case (GrpcException(GrpcStatus(`expectedCode`, _), _), None) => ()
      case (GrpcException(GrpcStatus(code, _), _), _) =>
        fail(s"Expected code [$expectedCode], but got [$code].")
      case (NonFatal(e), _) =>
        fail("Exception is neither a StatusRuntimeException nor a StatusException", e)
    }

  /** non-regex overload for assertGrpcError which just does a substring check.
    */
  def assertGrpcError(t: Throwable, expectedCode: Status.Code, pattern: String): Unit = {
    assertGrpcError(
      t,
      expectedCode,
      if (pattern.isEmpty) None else Some(Pattern.compile(Pattern.quote(pattern))),
    )
  }

  /** Allows for assertions with more information in the error messages. */
  implicit def futureAssertions[T](future: Future[T]): FutureAssertions[T] =
    new FutureAssertions[T](future)
}
