trait SAMUnit {
  def foo(a: Object): Unit
}

trait GenericSAM[R] {
  def foo(a: Object): R
}

object Test {
  val fun: Object => Unit = a => assert(a == "")
  val sam: SAMUnit = a => assert(a == "")
  val genericSam: GenericSAM[Unit] = a => assert(a == "")

  def main(args: Array[String]): Unit = {
    fun("")
    (fun: (Object => Any))("")
    sam.foo("")
    genericSam.foo("")
  }
}
