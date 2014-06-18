
object Test {

  type S = scala.Predef.Set

  val z: S = ???


  type Pair[T] = (T, T)
  val x = (1, 2)
  val xx: Pair[Int] = x
  val xxx = xx

  type Config[T] = (T => T, String)

  val y = ((x: String) => x, "a")
  val yy: Config[String] = y
  val yyy = yy

  type RMap[K, V] = Map[V, K]
  type RRMap[KK, VV] = RMap[VV, KK]

  val rm: RMap[Int, String] = Map[String, Int]()
  val rrm: RRMap[Int, String] = Map[Int, String]()

}
