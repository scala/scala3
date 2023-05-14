class Foo():
  def normal: Unit = println("normal")

extension (f: Foo)
  def ext: Unit = println("ext")

object Bar:
  def makeFoo[A]: Foo = Foo()
  def makeFoo[A](s: String): Foo = Foo()

def tests: Unit =
  Bar.makeFoo[Int].ext // error

  (Bar.makeFoo[Int]).ext // error

  val foo = Bar.makeFoo[Int]
  foo.ext // ok

  Bar.makeFoo[Int].normal // ok
