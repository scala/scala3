package concurrent

/** A hypothetical task scheduler trait */
trait Scheduler:
  def schedule(task: Runnable): Unit = ???

object Scheduler extends Scheduler:
  given fromAsync(using async: Async): Scheduler = async.scheduler
end Scheduler

