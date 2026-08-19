//> using options -Yexplicit-nulls
import language.experimental.magic
object Test extends App {
  case class Number(i: Int)

  object UnliftNumber {
    def unapply(t: Any): Number? = t match {
      case i: Int => Number(i)
      case _ => null
    }
  }

  def eval(expr: Any): Option[Number] = expr match {
    case UnliftNumber(n) => Some(n)
    case _ => None
  }

  println(eval(1))
}
