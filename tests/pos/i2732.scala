object Test {
  val f: java.util.function.Function[? >: String, ? <: Int] = str => 1

  val i: Int = f("")
}
