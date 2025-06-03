abstract class Parent {
  var number = 0

  def show: Unit
}

class Child extends Parent {
  number = 0

  println(show)

  def show = println(name)

  val name = "child"  // warn
}