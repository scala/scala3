// Check that exports do not leak private types
object Signature {

  private type T

  object O1 {
    private[Signature] def bar: T = ???
  }
  export O1._

  object O2 {
    private[Signature] val foo: T = ???
  }
  export O2._

  object PosTest:
    def main(args: Array[String]): Unit =
      val t1 = bar // ok
      val t2 = foo // ok
}

object Test:
  def main(args: Array[String]): Unit =
    val t1 = bar // error: Not found: bar
    val t2 = foo // error: Not found: foo
