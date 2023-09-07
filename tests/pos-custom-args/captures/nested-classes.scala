import language.experimental.captureChecking
import annotation.{capability, constructorOnly}

@capability class IO
class Blah
class Pkg(using @constructorOnly io: IO):
  class Foo:
    def m(foo: Blah^{io}) = ???
class Pkg2(using io: IO):
  class Foo:
    def m(foo: Blah^{io}): Any = io; ???

def main(using io: IO) =
  val pkg = Pkg()
  val f = pkg.Foo()
  f.m(???)
  val pkg2 = Pkg2()
  val f2 = pkg2.Foo()
  f2.m(???)


