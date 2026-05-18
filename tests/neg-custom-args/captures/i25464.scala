import language.experimental.captureChecking
import caps.*

trait CF1 extends Classifier, SharedCapability
trait CF2 extends CF1, Classifier

class C:
  class Impl(val a: C^{C.this}) extends CF2 // error
  val cf1: CF1 = ???
  val cf2 = new Impl(this)

def f(x: AnyRef^{any.only[CF2]}): Unit = ()

def test(c: C): Unit =
  f(c.cf1) // error
  f(c.cf2) // ok
  f(c.cf2.a.cf1) // error
  val d: AnyRef^{c.cf2} = c.cf2.a.cf1
  f(d) // wrong, passing CF1 to CF2 only

/*
  use http4s as DataBase, bar as Output, baz as HTTP

  foo.{A as B}

  [x: Ord as m]

  object Foo^{foo, http4s as HTTPClient, some DataBase}

  (using foo: Context)
  def goo(use Cpntext)

  uses DataBase
  uses {foo}

initialization:
  - start with main object
  - inside an object: initilize sequentially
  - along uses from main, with cycle detection
  - uses_init edges may not be part of cycles
  - keep bindings from capability class types to objects
  - uses x as X establishes and overwrites a binding




*/

