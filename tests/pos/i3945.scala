object Test {
  def int[A](k: String => A)(s: String)(x: Int): A = ???

  // composing directly: ok in scalac, error in dotc
  val c: (String => String) => (String) => (Int) => (Int) => String = (int[Int => String](_)).compose(int[String](_))

  // unwrapping composition: ok in scalac, ok in dotc
  val q: (String => Int => String) => (String) => (Int) => (Int => String) = int[Int => String]
  val p: (String => String) => (String) => (Int) => String = int[String]
  val c2: (String => String) => (String) => (Int) => (Int) => String = q.compose(p)
}
