import language.experimental.captureChecking
import language.experimental.modularity
import annotation.{capability, constructorOnly}

class IO extends caps.SharedCapability
class Blah
class Pkg(using tracked val io: IO):
  class Foo:
    def m(foo: Blah^{io}) = ???
class Pkg2(using tracked val io: IO):
  class Foo uses Pkg2.this.io:
    def m(foo: Blah^{io}): Any = io; ???

def main(using io: IO) =
  val pkg = Pkg()
  val f = pkg.Foo()
  f.m(???)
  val pkg2 = Pkg2()
  val f2 = pkg2.Foo()
  f2.m(???)


