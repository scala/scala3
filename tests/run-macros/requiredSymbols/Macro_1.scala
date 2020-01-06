import scala.quoted._

object Macro {
  inline def foo: String = ${ fooImpl }
  def fooImpl(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{given, _}
    val list = List(
      rootContext.requiredPackage("java"),
      rootContext.requiredPackage("java.lang"),
      rootContext.requiredPackage("scala"),
      rootContext.requiredPackage("scala.collection"),

      rootContext.requiredClass("java.lang.Object"),
      rootContext.requiredClass("scala.Any"),
      rootContext.requiredClass("scala.AnyRef"),
      rootContext.requiredClass("scala.AnyVal"),
      rootContext.requiredClass("scala.Unit"),
      rootContext.requiredClass("scala.Null"),

      rootContext.requiredModule("scala.None"),
      rootContext.requiredModule("scala.Nil"),

      rootContext.requiredMethod("scala.List.empty"),
    )
    Expr(list.map(_.fullName).mkString("\n"))
  }
}
