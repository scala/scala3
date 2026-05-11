import scala.quoted.*

object Macros:
  transparent inline def foo: String => String =
    ${ fooImpl }

  // Synthesizes x => x
  private def fooImpl(using Quotes): Expr[String => String] =
    import quotes.reflect.*

    val methSym =
      Symbol.newMethod(
        Symbol.spliceOwner,
        name = "fooImpl",
        tpe = MethodType(List("x"))(
          _ => List(TypeRepr.of[String]),
          _ => TypeRepr.of[String],
        ),
      )

    val meth =
      DefDef(
        methSym,
        rhsFn = { argss =>
          val List(List(x)) = argss
          Some(x.asInstanceOf[Term])
        }
      )

    Block(
      List(meth),
      Closure(Ident(methSym.termRef), tpe = None),
    ).asExprOf[String => String]
