import scala.quoted._

inline def findClass(inline path: String): String = ${ implClass('path) }
inline def findPackage(inline path: String): String = ${ implPackage('path) }
inline def findModule(inline path: String): String = ${ implModule('path) }

private def implClass(path: Expr[String])(using QuoteContext): Expr[String] =
  Expr(qctx.tasty.Type.requiredClassRef(path.unliftOrError).show)

private def implPackage(path: Expr[String])(using QuoteContext): Expr[String] =
  Expr(qctx.tasty.Type.requiredClassRef(path.unliftOrError).show)

private def implModule(path: Expr[String])(using QuoteContext): Expr[String] =
  Expr(qctx.tasty.Type.requiredClassRef(path.unliftOrError).show)
