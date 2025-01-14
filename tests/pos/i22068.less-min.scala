class A
class B extends A
class C extends A

object foos:
  opaque type Tag[A] = String
  object Tag:
    inline given mkTag[A]: Tag[A] = ???
    type Full[A] = Tag[A] | Set[A]
    sealed trait Set[A] extends Any
    case class Union[A](tags: Seq[Tag[Any]]) extends AnyVal with Set[A]:
      infix def and[B](t2: Full[B]): Unit = ???
    object Union:
      inline given mkUnion[A]: Union[A] = ???
import foos.Tag.*

class Test:
  inline def m1[K1, K2](using b1: Union[K1], b2: Union[K2]): Unit =
    b1.and(b2)

  def t1(): Unit = m1[B | C, A]
