trait Frag

case class IntFrag(x: Int) extends Frag
case class StringFrag(x: String) extends Frag

trait Fragable[T]:
  extension (x: T)
    def toFrags: List[Frag]

given Fragable[Int] =
  x => List(IntFrag(x))
given Fragable[String] =
  x => List(StringFrag(x))
given [A: Fragable] => Fragable[List[A]] =
  x => x.flatMap(_.toFrags)
given Fragable[EmptyTuple] =
  x => Nil
given [A: Fragable, B <: Tuple: Fragable] => Fragable[A *: B] =
  x => x.head.toFrags ++ x.tail.toFrags

def f[T: Fragable](x: T) =
  println(s"got: ${x.toFrags.mkString(", ")}")

@main def Test =
  f(1)
  f("abc")
  f(List(1, 2, 3))
  f(1, "a", List("c", "d"))
