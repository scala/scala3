import scala.quoted.*

object Companion {
  def fun(first: String): String = "anything"
}

object Macro {
  inline def mac(inline tree: String): String = ${ macImpl('tree) }
  def macImpl(tree: Expr[String])(using Quotes): Expr[String] = {
    tree match {
      case vv @ '{ ($s: Companion.type).fun($arg) } => arg
      case _ => ???
    }
  }
}
