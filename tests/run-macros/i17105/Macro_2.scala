import scala.quoted.*
import language.experimental.erasedDefinitions

inline def testExpr(inline body: Any) = ${ testExprImpl('body) }
def testExprImpl(body: Expr[Any])(using Quotes): Expr[String] =
  body match
    case '{ def g(y: String) = "placeholder" + y; $a(g): String } =>
      '{ $a((z: String) => s"[1st case] ${z}") }
    case '{ def g(y: String)(z: String) = "placeholder" + y; $a(g): String } =>
      '{ $a((z1: String) => (z2: String) =>  s"[2nd case] ${z1}, ${z2}") }
    // Refined Types
    case '{
      type t
      def refined(a: `t`): String = $x(a): String
      $y(refined): String
    } =>
      '{ $y($x) }
    // Dependent Types
    case '{
      def p(dsl: DSL): dsl.N = dsl.zero
      $y(p): String
    } =>
      '{ $y((dsl1: DSL) => dsl1.next(dsl1.zero)) }
    case '{
      def p(dsl: DSL)(a: dsl.N): dsl.N = a
      $y(p): String
    } =>
      '{ $y((dsl: DSL) => (b2: dsl.N) => dsl.next(b2)) }
    case '{
      def p(dsl1: DSL)(dsl2: DSL): dsl2.N = dsl2.zero
      $y(p): String
    } =>
      '{ $y((dsl1: DSL) => (dsl2: DSL) => dsl2.next(dsl2.zero)) }
    case _ => Expr("not matched")
