object Test extends App {

  case class C(x: Int)
  type IF[T] = given C => T

  val x: IF[Int] = implicitly[C].x

  val xs0: List[IF[Int]] = List(given _ => x)
  val xs: List[IF[Int]] = List(x)
  val ys: IF[List[Int]] = xs.map(x => x)
  val zs = ys given C(22)
  assert(zs == List(22))
}
