trait P:
  def foo: Int

class A extends P:
  export this.foo // error

trait Q extends P:
  def bar: Int

trait R extends P:
  def baz: Int
  val a1: A
  val a2: A

class B extends R:
  self =>
    export this.baz     // error
    export self.bar     // error
    export this.a1.foo
    export self.a2.foo  // error
    export a2.foo       // error