package com.daml.platform.apiserver

import java.util.concurrent._

import com.codahale.metrics.{InstrumentedExecutorService, InstrumentedThreadFactory}
import com.daml.metrics.Metrics
import com.daml.platform.NamedThreadFactory
import com.daml.platform.configuration.CommandConfiguration

import scala.concurrent.ExecutionContext

class ExecutionContexts(
                         val commandExecution: ExecutorService,
                         val monadicCommandExecutionContext: ExecutionContext,
                         val apiReadExecutionContext: ExecutionContext,
)

object ExecutionContexts {
  def fromConfig(config: CommandConfiguration, apiReadThreadPoolSize: Int, metrics: Metrics) = {
    // This is the main execution context for commands that limits the number of parallel
    // command interpretations via the number of available threads. Additional commands are being
    // queued in a bounded queue. If the queue is full, any further command is rejected with a
    // RESOURCE_EXHAUSTED error (i.e. back pressure signal)
    val commandExecutionThreads = {
      val prefix = "submission-service-command-execution"
      val threadName = s"$prefix-thread"
      val executorName = s"$prefix-executor"
      new InstrumentedExecutorService(
        new ThreadPoolExecutor(
          config.maxParallelSubmissions,
          config.maxParallelSubmissions,
          1,
          TimeUnit.MINUTES,
          new ArrayBlockingQueue[Runnable](config.inputBufferSize),
          new InstrumentedThreadFactory(
            new NamedThreadFactory(threadName),
            metrics.registry,
            threadName),
          new ThreadPoolExecutor.AbortPolicy(),
        ),
        metrics.registry,
        executorName
      )
    }
    // This executor is being used for monadic operations on futures (flatMap, transform).
    // We block in the main executor to make use of the already implemented scheduling mechanism in the
    // JDK libraries.
    val commandExecutionExecutionContext =
      ExecutionContext.fromExecutor(Executors.newWorkStealingPool(config.maxParallelSubmissions))

    val apiReadExecutionContext = {
      val prefix = "api-read-work-pool"
      val threadName = s"$prefix-thread"
      val executorName = s"$prefix-executor"
      ExecutionContext.fromExecutor(
        new InstrumentedExecutorService(
          Executors.newFixedThreadPool(
            apiReadThreadPoolSize,
            new InstrumentedThreadFactory(new NamedThreadFactory(threadName), metrics.registry, threadName)),
          metrics.registry,
          executorName,
        )
      )
    }

    new ExecutionContexts(commandExecutionThreads, commandExecutionExecutionContext, apiReadExecutionContext)
  }
}
