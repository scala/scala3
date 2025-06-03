class Foo {
  val x = "hello"
  val fun1: Int => Int = n => 0 + n + list.size
  val fun2: Int => Int = n => 1 + n + list.size
  fun2(5)

  List(5, 9).map(n => 2 + n + list.size)         // warn

  final val list = List(1, 2, 3)                 // warn

  List(5, 9).map(n => 3 + n + list.size)
}