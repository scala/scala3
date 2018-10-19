object Test {

  trait T {
    type A
  }

  class C extends T { this: m.type => // error: cannot instantiate
    type A >: Int | Test.A <: Int & Test.A

    def reveal(x: A): Int = x
    def inject(x: Int): A = x

    var a: A = ???
    x = a // OK!
    a = x // OK!
  }

  val m: T = new C
  type A = m.A

  var x: A = ???
}
