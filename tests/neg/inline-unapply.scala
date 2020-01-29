object Test {

  class C(val x: Int, val y: Int)

  inline def unapply(c: C): Some[(Int, Int)] = Some((c.x, c.y)) // error: Implementation restriction: inline unapply methods are not supported

}
object Test2 {

  class C(x: Int, y: Int)

  inline def unapply(c: C): Option[(Int, Int)] = inline c match { // error: Implementation restriction: inline unapply methods are not supported
    case x: C => Some((1, 1))
  }
}