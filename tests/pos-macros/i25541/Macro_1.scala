import scala.deriving.Mirror
import scala.quoted.*

object LocalMacros:
  def macroRWImpl[T: Type](using Quotes): Expr[Unit] =
    import quotes.reflect.*

    def inspect(tpe: TypeRepr, typeArgs: List[TypeRepr]): Unit =
      val constructorSym = tpe.typeSymbol.primaryConstructor
      val constructorParamSymss = constructorSym.paramSymss
      val (tparams0, params0) = constructorParamSymss.flatten.partition(_.isType)
      val constructorTpe = tpe.memberType(constructorSym).widen
      params0.foreach { param =>
        constructorTpe.memberType(param).substituteTypes(tparams0, typeArgs)
      }

    TypeRepr.of[T] match
      case applied: AppliedType => inspect(TypeRepr.of[T], applied.args)
      case _ => inspect(TypeRepr.of[T], Nil)

    '{ () }

trait Custom:
  inline def macroRW[T](using Mirror.Of[T]): Unit = ${ LocalMacros.macroRWImpl[T] }
