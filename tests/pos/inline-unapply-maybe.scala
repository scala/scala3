//> using options -Yexplicit-nulls
import language.experimental.errorHandling
object Test {

  class C(val x: Int, val y: Int)

  inline def unapply(c: C): Some[(Int, Int)] = Some((c.x, c.y))

}
object Test2 {

  class C(x: Int, y: Int)

  inline def unapply(c: C): (Int, Int)? = inline c match {
    case x: C => (1, 1)
  }
}
