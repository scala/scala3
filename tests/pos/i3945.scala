object Test {
  def int[A](k: String => A)(s: String)(x: Int): A = ???

  // composing directly: ok in scalac, now also in dotc
  val c: (String => String) => (String) => (Int) => (Int) => String = (int[Int => String](_)).compose(int[String](_))

  // unwrapping composition: ok in scalac, ok in dotc
  val q: (String => Int => String) => (String) => (Int) => (Int => String) = int[Int => String]
  val p: (String => String) => (String) => (Int) => String = int
  val c2: (String => String) => (String) => (Int) => (Int) => String = q.compose(p)

  class B
  class C extends B
  implicit def iC: C => Unit = ???

  // making sure A is not instantiated before implicit search
  def f[A](k: String => A)(s: String)(x: Int)(implicit y: A => Unit): A = ???
  val r: (String => C) => (String) => (Int) => B = f
}
