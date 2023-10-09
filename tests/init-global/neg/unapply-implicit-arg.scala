class Foo

object Bar {
  def unapply(using Foo)(pair: (Int, Int))(using Foo): Option[Int] =
    if pair._1 == 0 then Some(pair._1) else Some(pair._2)
  given Foo = new Foo
  val i1: Int = 0
  val i2: Int = (i1, i2) match // error
    case Bar(i) => i
    case _ => 0
}