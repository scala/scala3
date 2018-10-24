class Parent {
  val child: Child = new Child(this)
  child.number = 5

  val name = "parent"
}

class Child(parent: Raw[Parent]) {
  val name = "child"
  var number = 0

  println(parent.name) // error

  def show = {
    println(parent.name)
    println(name)
  }
}