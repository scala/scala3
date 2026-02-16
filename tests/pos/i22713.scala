import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit

def Test = {
  val worker: ScheduledExecutorService = ???
  object Worker extends Runnable {
    def fails(): Either[Exception, Unit] = Right(worker.schedule(this, 5, TimeUnit.NANOSECONDS))
    def works(): Either[Exception, Unit] = Right {
      worker.schedule(this, 5, TimeUnit.NANOSECONDS); ()
    }
    def run(): Unit = ???
  }
}
