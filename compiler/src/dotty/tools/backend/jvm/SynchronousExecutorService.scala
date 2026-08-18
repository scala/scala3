package dotty.tools.backend.jvm

import java.util.concurrent.{AbstractExecutorService, TimeUnit}

/** ExecutorService that runs everything synchronously, unlike a thread pool with n=1 that would still run in the background. */
final class SynchronousExecutorService extends AbstractExecutorService {
  private var hasShutdown = false

  override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean =
    true

  override def isShutdown: Boolean =
    hasShutdown

  override def isTerminated: Boolean =
    hasShutdown

  override def execute(command: Runnable): Unit =
    command.run()

  override def shutdown(): Unit =
    hasShutdown = true

  override def shutdownNow(): java.util.List[Runnable] =
    hasShutdown = true
    java.util.List.of()
}
