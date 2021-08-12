import scala.quoted.*

object TypeToolbox {
  inline def show[A <: AnyKind]: String = ${ showImpl[A] }
  private def showImpl[A <: AnyKind: Type](using Quotes) : Expr[String] =
    Expr(Type.show[A])

  inline def showStructure[A <: AnyKind]: String = ${ showStructureImpl[A] }
  private def showStructureImpl[A <: AnyKind](using q: Quotes, a: Type[A]) : Expr[String] = {
    import q.reflect._
    Expr(TypeRepr.of[A].show(using Printer.TypeReprStructure))
  }
}
