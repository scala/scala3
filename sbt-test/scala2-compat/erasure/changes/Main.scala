object Main {
  def main(args: Array[String]): Unit = {
    val z = new scala2Lib.Z

    def dummy[T]: T = null.asInstanceOf[T]

    // None of these method calls should typecheck, see `Scala2Erasure#supportedType`
    z.b_04(dummy)
    z.b_04X(dummy)
    z.b_05(dummy)
    z.a_48(dummy)
    z.c_49(dummy)
    z.a_51(dummy)
    z.a_53(dummy)
    z.b_56(dummy)
    z.a_57(dummy)
  }
}
