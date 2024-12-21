trait AnyFreeSpecLike:
  inline implicit def convertToFreeSpecStringWrapper(s: String): FreeSpecStringWrapper = ???
  protected final class FreeSpecStringWrapper(string: String):
    infix def in(testFun: => Any): Unit = ???


import types.Tag.*
class TagTest extends AnyFreeSpecLike{
  inline def test[T1, T2](using k1: Union[T1], k2: Union[T2]): Unit =
    "T1 <:< T2" in {
        val kresult = k1 <:< k2
        ???
    }
  class A
  class B extends A
  class C extends A
  test[B | C, A]
}

object types:
  opaque type Tag[A] = String
  object Tag:
    inline given apply[A]: Tag[A] = ???
    type Full[A] = Tag[A] | Set[A]
    sealed trait Set[A] extends Any
    case class Union[A](tags: Seq[Tag[Any]]) extends AnyVal with Set[A]:
      infix def <:<[B](t2: Full[B]): Boolean = ???
    object Union:
      inline given apply[A]: Union[A] = ???
