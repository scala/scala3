trait P:
  def foo: Int

class A extends P:
  export this.foo       // error

trait Q extends P:
  def bar: Int

trait R extends P:
  def baz: Int
  val a1: A
  val a2: A

abstract class B extends R:
  self =>
    export this.baz     // error
    export self.bar     // error
    export this.a1.foo
    export self.a2.foo  // error
    export a2.foo       // error

abstract class D extends P:
  val p: P
  export p.foo

abstract class E:
  self: P =>
    export self.foo     // error

abstract class F:
  self: P =>
    export this.foo     // error

class G(p: P):
  self: P =>
    export p.foo

class H(p: P):
  self: P =>
    export this.p.foo