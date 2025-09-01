trait TC[T] { def x: Int; def y: Int = 0 }

given [T] => TC[T]:
  inline val x = 1

given TC[Int]:
  inline val x = 2
  inline override val y = 3

object Test extends App {
  val z: 2 = summon[TC[Int]].x
  val _: 3 = summon[TC[Int]].y
}