object foos:
  opaque type Foo[T] = String
  object bars:
    class Bar1[A] { def and(b: Bar2): Unit = () }
    class Bar2
    inline def mkBar1[A]: Bar1[A] = new Bar1[A]
           def mkBar2   : Bar2    = new Bar2
import foos.*, bars.*

class Test:
  inline def m1[X](b1: Bar1[X], b2: Bar2): Unit =
    b1.and(b2)

  def t1(): Unit = m1(mkBar1[Int], mkBar2)
