//> using options -Yexplicit-nulls
import language.experimental.magic
object Unpack {
  (1, 2) match {
    case Unpack(first, _) => first
  }
  def unapply(e: (Int, Int)): T? = ??? // error
}