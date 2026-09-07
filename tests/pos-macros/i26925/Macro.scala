package a

import scala.quoted.*

class Personality(pars: DynamicWalkParameters)

object Macros {
  private def enumerateSubclassesImpl[T: Type](using q: Quotes): Expr[Unit] = {
    import q.reflect.*
    def visit(t: TypeRepr): Unit =
      t match
        case AppliedType(_, args) =>
          args.foreach(visit)
        case _ =>
          if t.typeSymbol.primaryConstructor.exists then
            println(t)
            val constructor = t.typeSymbol.primaryConstructor
            constructor.paramSymss.flatten.foreach { parameter =>
              visit(t.memberType(parameter))
            }

    visit(TypeRepr.of[T])
    '{()}
  }
  inline def enumerateSubclasses[T]: Unit = ${ enumerateSubclassesImpl[T] }
}
