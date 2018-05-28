object Test {
  val f: java.util.function.Function[_ >: String, _ <: Int] = str => 1

  val i: Int = f("")
}
