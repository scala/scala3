import scala.quoted.*
import scala.reflect.NameTransformer

inline def getFullName[T](inline decoded: Boolean) = ${ Macro.getFullName[T]('decoded) }

object Macro:
  def getFullName[T: Type](decodedExpr: Expr[Boolean])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val decode = decodedExpr.valueOrAbort

    val tpe = TypeRepr.of[T]

    val cls = tpe.classSymbol match
      case Some(cls) => cls
      case _ =>
        report.error("Not a class")
        return '{"<?>"}

    val name = cls.fullName

    if decode then
      val parts = name.split('.').toSeq
      val decoded = (parts.init.map(NameTransformer.decode) :+ parts.last).mkString(".")
      Expr(decoded)
    else
      Expr(name)
