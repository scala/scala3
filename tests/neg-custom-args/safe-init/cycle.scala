import scala.annotation.filled

class Parent {
  val child: Child @filled = new Child(this)
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