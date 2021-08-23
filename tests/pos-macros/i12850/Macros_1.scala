
import scala.quoted.*

inline def foo() = ${ expr }

private def expr(using Quotes): Expr[Unit] =
  import quotes.reflect.*

  // Option(1) match
  //   case Some(1) => ()
  //   case None => ()
  val mtch2 = Match(
      Apply(TypeApply(Ref(Symbol.requiredMethod("scala.Option.apply")), List(Inferred(TypeRepr.of[Int]))), List(Literal(IntConstant(1)))),
      List(
        CaseDef(/** FIXME: needs TypedTree from #12200; remove cast */Typed(Unapply(TypeApply(Ref(Symbol.requiredMethod("scala.Some.unapply")), List(Inferred(TypeRepr.of[Int]))), Nil, List(Literal(IntConstant(1)))).asInstanceOf[Term], Inferred(TypeRepr.of[Some[Int]])), None, Literal(UnitConstant())),
        CaseDef(Ref(Symbol.requiredModule("scala.None")), None, Literal(UnitConstant())))
  )

  mtch2.asExprOf[Unit]
