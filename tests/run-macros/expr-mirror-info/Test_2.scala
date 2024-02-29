import scala.deriving.Mirror

case class Foo[T](x: Int, y: Double, z: T)
enum Bar:
  case A
  case B(b: Int)

@main def Test: Unit =
  println(reflectMirrorInfo[Foo[Long]])
  println(reflectMirrorInfo2[Foo[Long]])
  println(reflectMirrorInfo[Bar])
  println(reflectMirrorInfo2[Bar])
