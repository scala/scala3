trait Foo[A]
trait Bar[B] extends Foo[B]

class Test[C]:
  def put[X >: Bar[C]](fn: X => Unit, x1: X): Unit = ()
  def id(foo: Foo[C]): Foo[C] = foo

  def t1(foo2: Foo[C]): Unit =
    put(id, foo2) // was: error: exp: Bar[C], got (foo2 : Foo[C])
