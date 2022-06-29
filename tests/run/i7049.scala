import scala.deriving._

case class Foo(x: Int, y: String)

def toTuple[T <: Product](x: T)(using m: Mirror.ProductOf[T], mt: Mirror.ProductOf[m.MirroredElemTypes]) =
  mt.fromProduct(x)

@main def Test = {
  val m = summon[Mirror.ProductOf[Foo]]
  val mt1 = summon[Mirror.ProductOf[(Int, String)]]
  type R = (Int, String)
  val mt2 = summon[Mirror.ProductOf[R]]
  val mt3 = summon[Mirror.ProductOf[m.MirroredElemTypes]]

  val f = Foo(1, "foo")
  val g: (Int, String) = toTuple(f)// (using m, mt1)
  assert(g == (1, "foo"))
}
