import scala.quoted._

object Macro {

  // By name StringContext is used to elide the prefix
  extension (inline sc: StringContext) inline def ff (args: => Any*): String = ${ Macro.impl('sc, 'args) }

  def impl(using s: Scope)(sc: s.Expr[StringContext], args: s.Expr[Seq[Any]]): s.Expr[String] = '{ $args.mkString }
}
