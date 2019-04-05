object Test extends App {
  inline def v[T] = valueOf[T]  // error

  println(v[String])
}