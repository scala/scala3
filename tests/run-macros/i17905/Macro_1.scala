import scala.quoted.*

inline def testCtxParam(inline body: Any) = ${ testCtxParamImpl('body) }
def testCtxParamImpl(body: Expr[Any])(using Quotes): Expr[String] =
  body match
    case '{ given i: String = "given"; def g(using s: String) = "placeholder"; $a(g, i): String } =>
      '{ $a(((s: String) ?=> s"[matched 1st case] ${s}"), "another_given") }
    case '{ def g(using s: String) = "placeholder"; $a(g): String } =>
      '{ $a((s: String) ?=> s"[matched 2nd case] ${s}") }
    case _ => Expr("not matched")
