trait Tr
enum Foo[T](x: T) {
  case Bar[T](y: T) extends Foo(y)
  case Bas[T](y: Int) extends Foo(y)
  case Bam[T](y: String) extends Foo(y) with Tr
  case Baz[S, T](y: String) extends Foo(y) with Tr
}
object Test {
  import Foo.*
  val bar: Foo[Boolean] = Bar(true)
  val bas: Foo[Int] = Bas(1)
  val bam: Foo[String] & Tr = Bam("")
  val baz: Foo[String] & Tr = Baz("")
}

enum Foo2[S <: T, T](x1: S, x2: T) {
  case Bar[T](y: T) extends Foo2(y, y)
  case Bas[T](y: Int) extends Foo2(y, y)
  case Bam[T](y: String) extends Foo2(y, y) with Tr
  case Baz[S, T](y: String) extends Foo2(y, y) with Tr
}
object Test2 {
  import Foo2.*
  val bar: Foo2[Boolean, Boolean] = Bar(true)
  val bas: Foo2[Int, Int] = Bas(1)
  val bam: Foo2[String, String] & Tr = Bam("")
  val baz: Foo2[String, String] & Tr = Baz("")
}


