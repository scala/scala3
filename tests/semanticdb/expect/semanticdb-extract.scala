object AnObject {

  def foo(x: Int) = ()
  def foo(): Unit = ()

  foo(1)
  foo()

  "".substring(1)
  "".substring(1, 2)

  List(1, 2)
  List.apply()
  List.`apply`()
  println(1 + 2)

  case class Foo(x: Int)
}
