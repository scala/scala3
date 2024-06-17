trait That1[A]
class T[A, This <: That1[A]](val x: Int) extends AnyVal {
  self: This =>
  final def loop(x: This, cnt: Int): Int = loop(x, cnt + 1)
  def const[B](): Boolean = return true
}

class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
  def baz[B >: A](x: B): List[B] = ???
}

object CollectionStrawMan {
  import collection.mutable.ArrayBuffer
  import reflect.ClassTag

  implicit class ArrayOps[A](val xs: Array[A]) extends AnyVal {

    def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

    protected[this] def newBuilder = new ArrayBuffer[A].mapResult(_.toArray(using elemTag))
  }
}

extension [A](xs: List[A])
  inline def foldl[B](z: B)(op: (B, A) => B): B =
    (xs: List[A]).foldLeft(z)(op)
  inline def concat[B <: A](ys: List[B]): List[A] = xs ++ ys

val x = List("a", "b").foldl[Int](0)((x, y) => x + y.length)
val y = Nil.concat(1 :: Nil)
val y1: List[Int] = y
val z = (1 :: Nil).concat(Nil)
val z1: List[Int] = z

trait TT:
  type A
  val m: A
  def f[B <: A](x: B): A = if ??? then m else x

extension (x: TT)
  def foo[B <: x.A](y: B) = x.f(y)

object CC extends TT:
  type A = Seq[Int]
  val m = Nil

val xx = CC.foo(List(1, 2, 3))

extension (x: TT)
  def ff[X](): Int = 1
  def ff[X](s: String): Int = s.length
  def ff[X](n: Int): Int = n

val yy =
  CC.ff[Int]()
  + CC.ff[String]("abc")
  + CC.ff[Int](22)
