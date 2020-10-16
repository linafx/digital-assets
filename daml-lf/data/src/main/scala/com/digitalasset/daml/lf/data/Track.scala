package com.daml.lf.data

import java.text.{DecimalFormat, NumberFormat}

import scala.collection.mutable
import scala.concurrent.Future

trait Track {
  def done(at: Long): Unit
  def took: Long
  def name: String
}

object TrackRunner extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  val x = SeqTrack("tests")
  x.startStep("y")
  x.done()
  Range(0, 1000).foreach(_ => 10 / 3)

  Range(0, 5000).foreach {
    _ => Future.successful {
      10 / 3
    }
  }
  Range(0, 5000).foreach {
    _ => Future {
      10 / 3
    }
  }


  val t = SeqTrack("tests")
  t.startStep("future.success")
  Range(0, 5000).foreach {
    _ => Future.successful {
      10 / 3
    }
  }
  t.startStep("future")
  Range(0, 5000).foreach {
    _ => Future {
      10 / 3
    }
  }
  t.startStep("throwing")
  Range(0, 5000).foreach {
    _ =>
      try {
        throw new Exception("x")
      } catch {
        case _: Throwable => ()
      }
  }
  t.startStep("throwing, but always the same")
  val e = new Exception
  Range(0, 5000).foreach {
    _ =>
      try {
        throw e
      } catch {
        case _: Throwable => ()
      }
  }
  t.startStep("nothing")
  Range(0, 1000000).foreach(_ => 10 / 3)
  t.startStep("rand")
  Range(0, 1000000).foreach(_ => Math.random())
  t.startStep("millis")
  Range(0, 1000000).foreach(_ => System.currentTimeMillis())
  val y = t.startSeq("nanos")
  y.startStep("1")
  Range(0, 500000).foreach(_ => System.nanoTime())
  y.startStep("2")
  Range(0, 500000).foreach(_ => System.nanoTime())
  t.done()
  println(Track.renderPlain(t))

  val tt = SeqTrack("some")
  tt.startStep("a")
  val ttt = tt.startSeq("thing")
  ttt.startStep("a")
  val tttt = ttt.startSeq("to")
  tttt.startStep("a")
  tttt.startStep("track")
  tttt.startStep("b")
  tttt.startStep("track")
  tttt.startStep("track")
  tttt.startStep("track2")
  tttt.startStep("track")
  tttt.startStep("track")
  tttt.startStep("track2")
  ttt.startStep("c")
  tt.startStep("c")
  tt.done()
  println(Track.renderPlain(tt))
  println(Track.renderPlain(tt, List("some.thing.to.track", "some.thing.to.track2")))

  val h = new HistoryTrack

  Range(1, 120).foreach { _ =>
    val s = SeqTrack("root")
    s.startStep("a")
    Thread.sleep((Math.random()*10).toLong)
    val ss = s.startSeq("b")
      ss.startStep("a")
    Thread.sleep((Math.random()*10).toLong)
      ss.startStep("b")
    Thread.sleep((Math.random()*10).toLong)
      ss.startStep("b")
    Thread.sleep((Math.random()*10).toLong)
      ss.startStep("b")
    Thread.sleep((Math.random()*10).toLong)
      ss.startStep("c")
    Thread.sleep((Math.random()*3).toLong)
    s.startStep("c")
    Thread.sleep((Math.random()*5).toLong)
    s.startStep("d")
    Thread.sleep((Math.random()*10).toLong)
    s.done()
    println(Track.renderPlain(s, List("root.b.b"), h.postFixRenderer))
    h.add(s)
  }

}

class HistoryTrack {

  private var i: Int = 0
  private var c: Int = 0
  private val h: mutable.Map[String, Array[Double]] = mutable.Map.empty

  def add(track: SeqTrack): Unit = synchronized {
    i = (i + 1) % 100
    c = c + 1
    h.valuesIterator.foreach(_(i) = 0)
    val total = track.took
    def rec(t: Track, prefix: String): Unit = {
      if (t != track) h.getOrElseUpdate(prefix + t.name, Array.ofDim(100))(i) += t.took.toDouble * 100 / total.toDouble
      t match {
        case t: SeqTrack => t.tracks.foreach(rec(_, s"$prefix${t.name}."))
        case _: StepTrack => // leaf
        case _: FlowTrack => // dive not supported
      }
    }
    rec(track, "")
  }

  def postFixRenderer: String => String = path => {
    h.get(path).map {
      a =>
        val sb = new mutable.StringBuilder()
        def percent(d: Double): String = {
          val plain = d.toInt.toString + "%"
          "    ".substring(plain.length) + plain
        }
        Iterator.iterate(i)(_ - 1)
          .take(5)
          .take(c)
          .map(x => if (x < 0) x + 100 else x)
          .map(a)
          .map(percent)
          .foreach(sb.append)
        if (c > 20)
          sb.append(" | 10: " + percent(Iterator.iterate(i)(_ - 1)
            .take(10)
            .map(x => if (x < 0) x + 100 else x)
            .map(a)
            .sum / 10))
        if (c > 110)
          sb.append(" | 100: " + percent(a.sum / 100))
        sb.toString()
    }.getOrElse("")
  }
}

object Track {

  val indent = "  "
  val namePad = 40

  private val nf = NumberFormat.getIntegerInstance.asInstanceOf[DecimalFormat]
  nf.setGroupingUsed(true)
  private val symbols = nf.getDecimalFormatSymbols
  symbols.setGroupingSeparator(' ')
  nf.setDecimalFormatSymbols(symbols)
  //             "999 999 999"
  // for terminal sorting val fullNano = "00000000000"
  val fullNano = "           "

  implicit class NanoRender(nano: Long) {
    def render: String = {
      val plain = nf.format(nano / 1000)
      s"${fullNano.substring(plain.length)}${plain}μ"
    }
  }

  def renderPlain(track: Track,
                  collecting: Seq[String] = Nil,
                  postFixRenderer: String => String = _ => ""): String = {
    val sb = new mutable.StringBuilder()
    val total = track match {
      case s: StepTrack => s.took
      case f: FlowTrack => f.took
      case s: SeqTrack => s.took
    }

    def rec(track: Track, indent: String, postfix: String, collect: Set[String], path: String): Unit = {
      val newPath = path + track.name
      def line(name: String, rest: Any): Unit = {
        sb.append(indent)
        sb.append(name.padTo(namePad, ' '))
        sb.append(rest)
        sb.append("\n")
        ()
      }

      def diveCollect(prefix: String): Set[String] =
        collect
          .filter(_.startsWith(prefix + "."))
          .map(_.drop(prefix.length + 1))

      track match {
        case Skipped(name) =>
          line(name, "SKIPPED" + postFixRenderer(newPath))

        case s: StepTrack =>
          line(s.name, s.took.render + postfix + postFixRenderer(newPath))

        case s: SeqTrack =>
          val newCollect = diveCollect(s.name)
          def percent(nano: Long): String = {
            val p = nano * 100 / total
            val s = s"$p%"
            "     ".substring(s.length) + s
          }
          line(s.name, s.took.render + postfix + postFixRenderer(newPath))
          s.tracks
            .filterNot(x => newCollect(x.name))
            .foreach(x => rec(x, indent + Track.indent, percent(x.took), newCollect, newPath + "."))
          s.tracks
            .filter(x => newCollect(x.name))
            .groupBy(_.name)
            .foreach {
              case (name, occurrences) =>
                val sum = occurrences.view.map(_.took).sum
                line(s"${Track.indent}[$name]", Track.indent + sum.render + percent(sum) + postFixRenderer(newPath + "." + name))
            }

        case f: FlowTrack =>
          line(f.name, f.took.render + postfix + postFixRenderer(newPath))
          val newCollect = diveCollect(f.name)
          f.flows.foreach(rec(_, indent + Track.indent, "", newCollect, newPath + "."))
      }
    }
    rec(track, indent, "", collecting.toSet, "")

    sb.toString()
  }

}

case class Skipped(name: String) extends Track {
  def done(at: Long): Unit = ()
  def took: Long = 0
}

case class StepTrack(name: String, start: Long) extends Track {
  var _done: Long = 0
  def done(at: Long = System.nanoTime()): Unit = if (_done == 0) {
    _done = at
  }
  def took: Long = _done - start
}

case class SeqTrack(name: String,
                    start: Long = System.nanoTime(),
                    tracks: mutable.ArrayBuffer[Track] = mutable.ArrayBuffer.empty) extends Track {
  var _done: Long = 0
  def took: Long = _done - start

  def done(at: Long = System.nanoTime()): Unit = if (_done == 0) {
    if (tracks.nonEmpty) tracks.last.done(at)
    _done = at
  }

  def startStep(name: String): SeqTrack = {
    val now = System.nanoTime()
    if (tracks.nonEmpty) tracks.last.done(now)
    tracks += StepTrack(name, now)
    this
  }

  def startSeq(name: String): SeqTrack = {
    val now = System.nanoTime()
    if (tracks.nonEmpty) tracks.last.done(now)
    val seqTrack = SeqTrack(name, now)
    tracks += seqTrack
    seqTrack
  }

  def startFlows(name: String): FlowTrack = {
    val now = System.nanoTime()
    if (tracks.nonEmpty) tracks.last.done(now)
    val flowTrack = FlowTrack(name, now)
    tracks += flowTrack
    flowTrack
  }
}

case class FlowTrack(name: String,
                     start: Long = System.nanoTime(),
                     flows: mutable.ArrayBuffer[SeqTrack] = mutable.ArrayBuffer.empty) extends Track {
  var _done: Long = 0
  def took: Long = _done - start

  def done(at: Long = System.nanoTime()): Unit = if (_done == 0) {
    _done = at
  }

  def startFlow: SeqTrack = {
    val now = System.nanoTime()
    flows.synchronized {
      val flow = SeqTrack(s"$name", now)
      flows += flow
      flow
    }
  }
}


