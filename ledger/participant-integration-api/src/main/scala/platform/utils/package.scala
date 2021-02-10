package com.daml.platform

import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Source}
import com.codahale.metrics.Counter

package object utils {
  implicit class `enriched flow`[I, O, M](flow: Flow[I, O, M]) {
    def backPressuredInstrumentedBuffer(size: Int, counter: Counter): Flow[I, O, M] =
      flow.map {
        el =>
          counter.inc()
          el
      }.buffer(size, OverflowStrategy.backpressure)
        .map { el =>
          counter.dec()
          el
        }
  }

  implicit class `enriched source`[O, M](source: Source[O, M]) {
    def backPressuredInstrumentedBuffer(size: Int, counter: Counter): Source[O, M] =
      source.map {
        el =>
          counter.inc()
          el
      }.buffer(size, OverflowStrategy.backpressure)
        .map { el =>
          counter.dec()
          el
        }
  }
}
