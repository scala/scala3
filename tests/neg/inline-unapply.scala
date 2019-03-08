object Test {

  class C(val x: Int, val y: Int)

  inline def unapply(c: C): Option[(Int, Int)] = Some((c.x, c.y)) // ok

}
object Test2 {

  class C(x: Int, y: Int)

  inline def unapply(c: C): Option[(Int, Int)] = inline c match { // error: inline match cannot be used in an inline unapply
    case x: C => (1, 1)
  }
}