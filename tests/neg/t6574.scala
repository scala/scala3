import annotation.tailrec

class Bad[X, Y](val v: Int) extends AnyVal {
  @tailrec final def notTailPos[Z](a: Int)(b: String): Unit = {
    this.notTailPos[Z](a)(b) // error: it is not in tail position
    println("tail")
  }

  @tailrec final def differentTypeArgs: Unit = {
    {(); new Bad[String, Unit](0)}.differentTypeArgs
  }
}
