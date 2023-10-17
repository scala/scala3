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

class BoxWithCompanion[+A](value: A) {
  private var cached: A = value // error
  def get: A = cached
}

class BoxValid[+A](value: A, orig: A) {
  private var cached: A = value // ok
  def get: A = cached

  def reset(): Unit =
    cached = orig // ok: mutated through this prefix
}

trait Animal
object Dog extends Animal
object Cat extends Animal

val dogBox: Box[Dog.type] = new Box(Dog)
val _ = dogBox.put(Cat)
val dog: Dog.type = dogBox.get


object BoxWithCompanion {
  def put[A](box: BoxWithCompanion[A], value: A): Unit = {
    box.cached = value
  }
}