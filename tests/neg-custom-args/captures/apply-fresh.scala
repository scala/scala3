object Test

class Ref

class Foo:
  def newRef: Ref^ = Ref()

class Bar:
  val newRef: Ref^ = Ref()

def newRef: Ref^ = Ref()

def test =

  val f = () => newRef
  val x = f()
  val _: Ref = x // error

  val foo = Foo()
  val y = foo.newRef
  val _: Ref = y // error

  val bar = Bar()
  val z = bar.newRef
  val _: Ref = z // error
