//> using options -Werror -deprecation -feature -Yexplicit-nulls

import language.experimental.magic
object Unapply {
  def unapply(a: Any): (Int, Int)? =
    (1, 2)
}

object Test {
  val Unapply(x, y) = "": @unchecked
}
