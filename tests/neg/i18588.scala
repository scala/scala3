class ROBox[+A](value: A) {
  private var cached: A = value
  def get: A = ROBox[A](value).cached
}

class Box[+A](value: A) {
  private var cached: A = value // error
  def get: A = cached

  def put[AA >: A](value: AA): Unit = {
    val box: Box[AA] = this
    box.cached = value
  }
}

trait Animal
object Dog extends Animal
object Cat extends Animal

val dogBox: Box[Dog.type] = new Box(Dog)
val _ = dogBox.put(Cat)
val dog: Dog.type = dogBox.get
