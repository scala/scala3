object Test extends dotty.runtime.LegacyApp {
  println(implicitly[C[Int]])
  println(implicitly[C[String]])
  println(implicitly[C[Nothing]])
}