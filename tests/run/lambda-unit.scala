trait SAMUnit {
  def foo(a: Object): Unit
}

object Test {
  val fun: Object => Unit = a => assert(a == "")
  val sam: SAMUnit = a => assert(a == "")

  def main(args: Array[String]): Unit = {
    fun("")
    (fun: Object => Any)("")
    sam.foo("")
  }
}
