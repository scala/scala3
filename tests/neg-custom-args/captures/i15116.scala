class Foo(m: {*} String)
class Bar(val m: {*} String):
  val x = Foo(m)   // error
trait Baz(val m: {*} String):
  val x = Foo(m)   // error
class Bar1(m: {*} String):
  val x = Foo(m)   // error
trait Baz2(m: {*} String):
  val x = Foo(m)   // error
