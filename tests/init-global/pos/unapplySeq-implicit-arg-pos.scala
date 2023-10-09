class Foo

object Bar {
  def unapplySeq(using Foo)(using Foo)(pair: (Int, Int))(using Foo): Option[Seq[Int]] =
    if pair._1 == 0 then Some(Seq(pair._1)) else Some(Seq(pair._2))
  given Foo = new Foo
  val i1: Int = 0
  val i2: Int = (i1, i1) match
    case Bar(i) => i
    case _ => 0
}
