import scala.quoted.*

inline def makeClass(inline name: String): Bar = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Bar] = {
  import quotes.reflect.*
  val name = nameExpr.valueOrAbort
  val fooDef = makeFoo()
  val fooBarDef = makeFooBar(name, fooDef.symbol)
  val newCls = makeNewFooBar(fooBarDef.symbol)

  Block(List(fooDef, fooBarDef), newCls).asExprOf[Bar]
  // '{
  //   class Foo { self: Bar =>
  //      def foo(): Unit = bar()
  //   }
  //   class `name`() extends Foo with Bar
  //   new `name`()
  // }
}

/** Generate
 *  ```
 *  class Foo { self: Bar =>
 *    def foo(): Unit = bar()
 *  }
 * ```
 */
def makeFoo(using Quotes)(): quotes.reflect.ClassDef = {
  import quotes.reflect.*
  val parents = List(TypeTree.of[Object])
  def decls(cls: Symbol): List[Symbol] =
    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))

  val cls = Symbol.newClass(Symbol.spliceOwner, "Foo", parents = parents.map(_.tpe), decls, selfType = Some(TypeRepr.of[Bar]))
  val fooSym = cls.declaredMethod("foo").head
  val barSym = Symbol.classSymbol("Bar").declaredMethod("bar").head

  def fooRhs(args: List[List[Tree]]): Option[Term] =
    val barCall = This(cls).select(barSym).appliedToNone.asExprOf[Unit]
    Some('{ println("Calling Foo.foo"); $barCall }.asTerm)

  val fooDef = DefDef(fooSym, fooRhs)
  ClassDef(cls, parents, body = List(fooDef))
}

/** Generate
 *  ```
 *  class `name`() extends Foo with Bar
 * ```
 */
def makeFooBar(using Quotes)(name: String, fooCls: quotes.reflect.Symbol): quotes.reflect.ClassDef = {
  import quotes.reflect.*
  val parents = List(TypeTree.ref(fooCls), TypeTree.of[Bar])
  def decls(cls: Symbol): List[Symbol] = Nil
  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
  ClassDef(cls, parents, body = Nil)
}

/** Generate
 *  ```
 *  new `name`()
 *  ```
 */
def makeNewFooBar(using Quotes)(fooBarCls: quotes.reflect.Symbol): quotes.reflect.Term = {
  import quotes.reflect.*
  Typed(Apply(Select(New(TypeIdent(fooBarCls)), fooBarCls.primaryConstructor), Nil), TypeTree.of[Bar])
}

trait Bar {
  def bar(): Unit = println("Calling Bar.bar")
}
