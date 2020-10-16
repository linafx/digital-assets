//package com.daml.lf.speedy
//
//import scala.concurrent.Future
//import scala.concurrent.ExecutionContext.Implicits.global
//
//object ThrowTracker {
//
//
//
//  var count = 0
//  private val window: Array[Int] = Array.ofDim(100)
//  private var windowIndex = 0
//
//  def poke(): Unit = ()
//
//  def inc(): Unit = this.synchronized {
//    count += 1
//  }
//
//  var dumpNum: Int = 0
//  private def dump(): String = {
//    val currentCount = this.synchronized {
//      val result = count
//      count = 0
//      dumpNum += 1
//      result
//    }
//    window.update(windowIndex, currentCount)
//    windowIndex = (windowIndex + 1) % 100
//    val tenSum = Range(0, 10).view
//      .map(i => ((windowIndex + 90) % 100 + i) % 100)
//      .map(window)
//      .sum
//    s"rolling average 100:${if (dumpNum < 110) "-" else window.sum / 100}/s  10:${if (dumpNum < 20) "-" else tenSum / 10}/s 1: $currentCount/s"
//  }
//
//  val neverEnd = Future {
//    while (true) {
//      Thread.sleep(1000)
//
//      println(s"speedy throws: ${dump()}")
//    }
//  }
//
//}
//
