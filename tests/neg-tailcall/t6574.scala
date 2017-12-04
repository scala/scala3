class Bad[X, Y](val v: Int) extends AnyVal {
  @annotation.tailrec final def notTailPos[Z](a: Int)(b: String): Unit = {   // error
    this.notTailPos[Z](a)(b)          // error
    println("tail")
  }

  @annotation.tailrec final def differentTypeArgs: Unit = { // error
    {(); new Bad[String, Unit](0)}.differentTypeArgs // error
  }
}
