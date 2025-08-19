package test
class STR
class Foo(m: STR^)
class Bar(val m: STR^):
  val x = Foo(m)   // error
trait Baz(val m: STR^):
  val x = Foo(m)   // error
class Bar1(m: STR^):
  val x = Foo(m)   // error
trait Baz2(m: STR^):
  val x = Foo(m)   // error
