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

    protected[this] def newBuilder = new ArrayBuffer[A].mapResult(_.toArray(elemTag))
  }
}
