import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation

@experimental
class check extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case DefDef(name, params, tpt, Some(fooTree)) =>
        val name: String = "myClass"
        val parents = List(TypeTree.of[Object])
        def decls(cls: Symbol): List[Symbol] =
          List(Symbol.newMethod(cls, "check", MethodType(List("x"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Unit])))
        val cls = Symbol.newClass(Symbol.spliceOwner.owner, name, parents = parents.map(_.tpe), decls, selfType = None)
        val checkSym = cls.declaredMethod("check").head
        def checkRhs(args: List[List[Tree]]): Option[Term] =
          val x = args.head.head.asExprOf[Int]
          Some('{println(s"Calling Check with ${$x}")}.asTerm)
        val checkDef = DefDef(checkSym, checkRhs)
        val clsDef = ClassDef(cls, parents, body = List(checkDef))

        val x = Ref(params.head.params.head.symbol)
        val callCheck = Apply(Select.unique(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), "check"), List(x)).asExprOf[Unit]
        val rhs = '{
          $callCheck
          ${fooTree.asExprOf[Int]}
        }.asTerm
        List(clsDef, DefDef.copy(tree)(name, params, tpt, Some(rhs)))
}
