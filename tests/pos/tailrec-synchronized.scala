import scala.annotation.tailrec

class TailrecProblem {
  @tailrec
  private def create(x: Int): Unit = {
    this.synchronized {
      if (x == 0) ()
      else create(x - 1)
    }
  }
}
