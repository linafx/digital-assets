// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.validator.caching

import com.daml.caching.{Cache, Weight}
import com.daml.ledger.participant.state.kvutils.DamlKvutils.{DamlStateKey, DamlStateValue}
import com.daml.ledger.participant.state.kvutils.Fingerprint
import com.daml.ledger.validator.StateKeySerializationStrategy
import com.daml.ledger.validator.caching.CachingDamlLedgerStateReaderWithFingerprints.StateCacheWithFingerprints
import com.daml.ledger.validator.preexecution.{DamlLedgerStateReaderWithFingerprints, LedgerStateReaderWithFingerprints, RawToDamlLedgerStateReaderWithFingerprintsAdapter}
import com.google.protobuf.MessageLite

import scala.concurrent.{ExecutionContext, Future}

/**
  * A caching adapter for ledger read operations that return fingerprints as well.
  * Caches only positive lookups, i.e., in case the values for a requested key are available on the ledger.
  */
class CachingDamlLedgerStateReaderWithFingerprints(
    val cache: StateCacheWithFingerprints,
    shouldCache: DamlStateKey => Boolean,
    delegate: DamlLedgerStateReaderWithFingerprints)(implicit executionContext: ExecutionContext)
    extends DamlLedgerStateReaderWithFingerprints {
  override def read(keys: Seq[DamlStateKey], validateCached: Seq[(DamlStateKey, Fingerprint)]): Future[Seq[(Option[DamlStateValue], Fingerprint)]] = {
    @SuppressWarnings(Array("org.wartremover.warts.Any")) // Required to make `.view` work.
    val cachedValues: Map[DamlStateKey, (Option[DamlStateValue], Fingerprint)] = keys.view
      .map(key => key -> cache.getIfPresent(key))
      .collect {
        case (key, Some((value, fingerprint))) => (key, (Some(value), fingerprint))
      }
      .toMap

    val (immutables, mutablesCached) = cachedValues.partition{case (key, _) => isImmutable(key)}

    val keysToRead = (keys.toSet -- cachedValues.keySet).toSeq
    val mutableValueQuerySeq = mutablesCached.view.map{case (k, (_, fp)) => k -> fp}.toSeq
    if (keysToRead.nonEmpty || mutableValueQuerySeq.nonEmpty) {
      for{
        values <- delegate.read(keysToRead, mutableValueQuerySeq)
      } yield {
        assert(keysToRead.size + mutableValueQuerySeq.size == values.size)
        val readValues = (keysToRead ++ mutableValueQuerySeq.map(_._1)).zip(values).toMap
        readValues.collect {
          case (key, (Some(value), fingerprint)) if shouldCache(key) =>
            cache.put(key, value -> fingerprint)
        }
        val all = immutables ++ mutablesCached ++ readValues
        keys.map(all(_))
      }
    } else {
      Future {
        keys.map(cachedValues(_))
      }
    }
  }

  private def isImmutable(key: DamlStateKey): Boolean =
    key.getPackageId.nonEmpty || key.getParty.nonEmpty
}

object CachingDamlLedgerStateReaderWithFingerprints {

  implicit object `Message-Fingerprint Pair Weight` extends Weight[(MessageLite, Fingerprint)] {
    override def weigh(value: (MessageLite, Fingerprint)): Cache.Size =
      value._1.getSerializedSize.toLong + value._2.size()
  }

  type StateCacheWithFingerprints = Cache[DamlStateKey, (DamlStateValue, Fingerprint)]

  def apply(
      cache: StateCacheWithFingerprints,
      cachingPolicy: CacheUpdatePolicy,
      ledgerStateReaderWithFingerprints: LedgerStateReaderWithFingerprints,
      keySerializationStrategy: StateKeySerializationStrategy)(
      implicit executionContext: ExecutionContext): CachingDamlLedgerStateReaderWithFingerprints =
    new CachingDamlLedgerStateReaderWithFingerprints(
      cache,
      cachingPolicy.shouldCacheOnRead,
      new RawToDamlLedgerStateReaderWithFingerprintsAdapter(
        ledgerStateReaderWithFingerprints,
        keySerializationStrategy)
    )
}
