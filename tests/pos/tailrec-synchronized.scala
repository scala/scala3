// scalajs: --skip
// (JVM-only simplification)

import scala.annotation.tailrec

class TailrecProblem {
  @tailrec
  private def create(x: Int): Unit = {
    this.synchronized {
      if (x == 0) ()
      else create(x - 1)
    }
  }

  @tailrec
  private def create2(x: Int): Unit = {
    this.synchronized {
      if (x == 0) ()
      else {
        val tmp = x - 1
        create2(tmp)
      }
    }
  }
}
