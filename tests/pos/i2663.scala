trait Tr
enum Foo[T](x: T) {
  case Bar[T](y: T) extends Foo(y)
  case Bas[T](y: Int) extends Foo(y)
  case Bam[T](y: String) extends Foo(y) with Tr
}
object Test {
  import Foo._
  val bar: Foo[Boolean] = Bar(true)
  val bas: Foo[Int] = Bas(1)
  val bam: Foo[String] & Tr = Bam("")
}


