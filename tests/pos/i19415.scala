def Test = {
  val left: Parser[String] = ???
  val right: Parser[Int] = ???
  val both = left && right

  val works = both.map(Ior.Both.apply)
  val fails = (left && right).map(Ior.Both.apply)
}

trait Parser[T]:
  final def &&[T2](other: Parser[T2])(implicit zip: Zip[T, T2]): Parser[zip.Out] = ???
  final def map[T2](f: T => T2): Parser[T2] = ???

infix trait Ior[+A, +B]
object Ior:
  final case class Both[+A, +B](a: A, b: B) extends (A Ior B)

trait Zip[In1, In2]:
  type Out

object Zip {
  type Out[In1, In2, O] = Zip[In1, In2] { type Out = O }
  implicit def zip2[_1, _2]: Zip.Out[_1, _2, (_1, _2)] = ???
}