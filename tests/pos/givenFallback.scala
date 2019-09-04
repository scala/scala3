trait TC[T] { def x: Int; def y: Int = 0 }

given [T] as TC[T] {
  inline val x = 1
}

given as TC[Int] {
  inline val x = 2
  inline override val y = 3
}

object Test extends App {
  val z: 2 = the[TC[Int]].x
  val _: 3 = the[TC[Int]].y
}