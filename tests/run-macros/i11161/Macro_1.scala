import scala.quoted._
import scala.reflect.ClassTag

inline def showType[T]: String = ${ showTypeImpl[T] }

private def showTypeImpl[T: Type](using Quotes): Expr[String] =
  Expr.summon[ClassTag[T]] match
      case Some(ct) => '{ $ct.runtimeClass.getName }
      case None =>
        import quotes.reflect._
        report.throwError(s"Unable to find a ClassTag for type ${Type.show[T]}", Position.ofMacroExpansion)
