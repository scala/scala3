def foo(x: Any { def foo: Int }): Any { val foo: Int } = x // error
def foo1(x: Any { val foo: Int }): Any { def foo: Int } = x // ok
def foo2(x: Any { val foo: Int }): Any { val foo: Int } = x // ok
def foo3(x: Any { def foo: Int }): Any { def foo: Int } = x // ok

class Foo:
  val foo: Int = 1
class Foo1:
  def foo: Int = 1
class Foo2:
  var foo: Int = 1

def foo4(x: Foo): Any { val foo: Int } = x // ok
def foo4(x: Foo1): Any { val foo: Int } = x // error
def foo4(x: Foo2): Any { val foo: Int } = x // error
