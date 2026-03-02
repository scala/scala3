import scala.quoted._

inline def makeClassAndCall(inline name: String, idx: Int, str: String): Unit = ${ makeClassAndCallExpr('name, 'idx, 'str) }
private def makeClassAndCallExpr(nameExpr: Expr[String], idxExpr: Expr[Int], strExpr: Expr[String])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort

  def decls(cls: Symbol): List[Symbol] =
    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
  val parents = List(TypeTree.of[Object])
  val cls = Symbol.newClass(
    Symbol.spliceOwner,
    name,
    parents = _ => parents.map(_.tpe),
    decls,
    selfType = None,
    clsFlags = Flags.EmptyFlags,
    Symbol.noSymbol,
    List(("idx", TypeRepr.of[Int]), ("str", TypeRepr.of[String]))
  )

  val fooSym = cls.declaredMethod("foo").head
  val idxSym = cls.fieldMember("idx")
  val strSym = cls.fieldMember("str")
  val fooDef = DefDef(fooSym, argss =>
    Some('{println(s"Foo method call with (${${Ref(idxSym).asExpr}}, ${${Ref(strSym).asExpr}})")}.asTerm)
  )
  val clsDef = ClassDef(cls, parents, body = List(fooDef))
  val newCls = Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List(idxExpr.asTerm, strExpr.asTerm))

  Block(List(clsDef), Apply(Select(newCls, cls.methodMember("foo")(0)), Nil)).asExprOf[Unit]

  // '{
  //   class `name`(idx: Int, str: String) {
  //     def foo() = println("Foo method call with ($idx, $str)")
  //   }
  //   new `name`(`idx`, `str`)
  // }
}
