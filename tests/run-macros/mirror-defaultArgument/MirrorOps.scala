import scala.deriving._
import scala.annotation.experimental
import scala.quoted._

object MirrorOps:

  inline def overridesDefaultArgument[T]: Boolean = ${ overridesDefaultArgumentImpl[T] }

  def overridesDefaultArgumentImpl[T](using Quotes, Type[T]): Expr[Boolean] =
    import quotes.reflect.*
    val cls = TypeRepr.of[T].classSymbol.get
    val companion = cls.companionModule.moduleClass
    val methods = companion.declaredMethods

    val experAnnotType = Symbol.requiredClass("scala.annotation.experimental").typeRef

    Expr {
      methods.exists { m =>
        m.name == "defaultArgument" &&
        m.flags.is(Flags.Synthetic) &&
        m.annotations.exists(_.tpe <:< experAnnotType)
      }
    }

end MirrorOps
