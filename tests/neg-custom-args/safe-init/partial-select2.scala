abstract class Parent {
  var number = 0

  def show: Unit
}

class Child extends Parent {
  number = 0

  println(show)                // error

  def show = println(name)     // error

  val name = "child"
}