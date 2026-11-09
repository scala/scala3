object Macro {
  import scala.quoted.*
  inline def myMacro(): Unit = ${impl()}
  def impl()(using Quotes): Expr[Unit] = '{ () }
}
