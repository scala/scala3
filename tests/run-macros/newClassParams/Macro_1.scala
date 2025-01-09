//> using options -experimental

import scala.quoted._

inline def makeClassAndCall(inline name: String, idx: Int, str: String): Unit = ${ makeClassAndCallExpr('name, 'idx, 'str) }
private def makeClassAndCallExpr(nameExpr: Expr[String], idxExpr: Expr[Int], strExpr: Expr[String])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort

  def decls(cls: Symbol): List[Symbol] = List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
  val parents = List(TypeTree.of[Object])
  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = _ => parents.map(_.tpe), decls, selfType = None, Flags.EmptyFlags, Symbol.noSymbol, List("idx", "str"), List(TypeRepr.of[Int], TypeRepr.of[String]))

  val fooDef = DefDef(cls.methodMember("foo")(0), argss => Some('{println(s"Foo method call with (${${Ref(cls.fieldMember("idx")).asExpr}}, ${${Ref(cls.fieldMember("str")).asExpr}})")}.asTerm))
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
