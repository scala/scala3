package typespatmat

sealed trait Box2[T]
final case class Int2(x: Int) extends Box2[Int]
final case class Str2(x: String)
                              extends Box2[String]
final case class Gen[T](x: T) extends Box2[T]

object Box2 {
  def double2[T](x: Box2[T]): T = x match {
    case Int2(i) => i * 2
    case Str2(s) => s + s
    case Gen(x) => x
  }
}
