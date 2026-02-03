import language.experimental.captureChecking
import annotation.{capability, constructorOnly}

class IO extends caps.SharedCapability
class Blah
class Pkg(using io: IO):
  class Foo:
    this: Foo^{Pkg.this} =>
    def m(foo: Blah^{io}) = ???
class Pkg2(using io: IO):
  class Foo uses Pkg2.this.io:
    def m(foo: Blah^{io}): Any = io; ???

def main(using io: IO) =
  val pkg = Pkg()
  val f = pkg.Foo()
  val x1 = f.m(???)
  val pkg2 = Pkg2()
  val f2 = pkg2.Foo()
  val x2 = f2.m(???)


