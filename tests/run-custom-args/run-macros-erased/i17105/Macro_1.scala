import scala.quoted.*

inline def testExpr(inline body: Any) = ${ testExprImpl('body) }
def testExprImpl(body: Expr[Any])(using Quotes): Expr[String] =
  body match
    // Erased Types
    case '{ def erasedfn(y: String) = "placeholder"; $a(erasedfn): String } =>
      Expr("This case should not match")
    case '{ def erasedfn(erased y: String) = "placeholder"; $a(erasedfn): String } =>
      '{ $a((erased z: String) => "[erased case]") }
    case '{
      def erasedfn(a: String, b: String)(c: String, d: String): String = a
      $y(erasedfn): String
    } => Expr("This should not match")
    case '{
      def erasedfn(a: String, erased b: String)(erased c: String, d: String): String = a
      $y(erasedfn): String
    } =>
      '{ $y((a: String, erased b: String) => (erased c: String, d: String) => d) }
    case '{
      def erasedfn(a: String, erased b: String)(c: String, erased d: String): String = a
      $y(erasedfn): String
    } =>
      '{ $y((a: String, erased b: String) => (c: String, erased d: String) => c) }
    case _ => Expr("not matched")
