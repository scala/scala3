class Foo {
  val x = "hello"
  val fun1: Int => Int = n => 0 + n + list.size    // ok
  val fun2: Int => Int = n => 1 + n + list.size    // error: fun is called in the next line
  fun2(5)                                          // error: latent effects

  List(5, 9).map(n => 2 + n + list.size)         // error: closure is partial, but a full value expected

  val list = List(1, 2, 3)

  List(5, 9).map(n => 3 + n + list.size)         // ok, `this.list` already initialized
}