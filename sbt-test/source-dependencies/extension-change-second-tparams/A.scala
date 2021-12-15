object A {

  class Box[T](val value: T)

  extension (box: Box[Int]) def map[I <: Int](f: Int => I): Box[I] = new Box(f(box.value))

}
