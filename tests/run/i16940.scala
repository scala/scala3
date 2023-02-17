// scalac: -coverage-out:coverage
// scalajs: --skip

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.*

var test = 0

def brokenSynchronizedBlock(option: Boolean): Future[Unit] = Future {
  if (option) {
    Thread.sleep(500)
  }
  synchronized {
    val tmp = test
    Thread.sleep(1000)
    test = tmp + 1
  }
}

object Test extends App {
  Await.result(
    Future.sequence(Seq(brokenSynchronizedBlock(false), brokenSynchronizedBlock(true)))
      .map { result =>
        println(test)
        assert(test == 2)
      },
    3.seconds
  )
}
