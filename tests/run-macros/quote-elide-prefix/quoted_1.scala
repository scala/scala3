import scala.quoted.*

object Macro {

  // By name StringContext is used to elide the prefix
  extension (inline sc: StringContext) inline def ff (args: => Any*): String = ${ Macro.impl('sc, 'args) }

  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[String] = '{ $args.mkString }
}
