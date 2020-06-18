package com.daml.platform

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

class NamedThreadFactory(name: String) extends ThreadFactory {
  private val group = {
    val s: SecurityManager = System.getSecurityManager
    if (s != null) s.getThreadGroup
    else Thread.currentThread.getThreadGroup
  }
  private val threadNumber = new AtomicInteger(1)
  private val namePrefix = s"$name-"

  override def newThread(r: Runnable): Thread = {
    val t = new Thread(group, r, namePrefix + threadNumber.getAndIncrement.toString, 0)
    if (t.isDaemon) t.setDaemon(false)
    if (t.getPriority != Thread.NORM_PRIORITY) t.setPriority(Thread.NORM_PRIORITY)
    t
  }
}