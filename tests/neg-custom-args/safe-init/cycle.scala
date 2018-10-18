class Parent {
  val child: Child = new Child(this)
  child.show   // error

  val name = "parent"

  def show = child.show
}

class Child(parent: Partial[Parent]) {
  val name = "child"

  println(parent.name) // error

  def show = {
    println(parent.name)
    println(name)
  }
}