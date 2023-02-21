package concurrent

/** A hypothetical task scheduler trait */
trait Scheduler:
  def schedule(task: Runnable): Unit = task.run()

object Scheduler extends Scheduler:
end Scheduler

