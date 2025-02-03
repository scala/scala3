//> using options -experimental

import scala.quoted.*

transparent inline def makeClass(): Any = ${ makeClassExpr }
private def makeClassExpr(using Quotes): Expr[Any] = {
  import quotes.reflect.*

  val name = "myClass"
  def decls(cls: Symbol): List[Symbol] =
    List(Symbol.newMethod(cls, "getParam", MethodType(Nil)(_ => Nil, _ => cls.typeMember("T").typeRef)))
  val conMethodType =
    (classType: TypeRepr) => PolyType(List("T"))(_ => List(TypeBounds.empty), polyType =>
      MethodType(List("param"))((_: MethodType) => List(polyType.param(0)), (_: MethodType) =>
        AppliedType(classType, List(polyType.param(0)))
      )
    )
  val cls = Symbol.newClass(
    Symbol.spliceOwner,
    name,
    parents = _ => List(TypeRepr.of[Object]),
    decls,
    selfType = None,
    clsFlags = Flags.EmptyFlags,
    clsPrivateWithin = Symbol.noSymbol,
    clsAnnotations = Nil,
    conMethodType,
    conFlags = Flags.EmptyFlags,
    conPrivateWithin = Symbol.noSymbol,
    conParamFlags = List(List(Flags.EmptyFlags), List(Flags.EmptyFlags)),
    conParamPrivateWithins = List(List(Symbol.noSymbol), List(Symbol.noSymbol))
  )

  val getParamSym = cls.declaredMethod("getParam").head
  def getParamRhs(): Option[Term] =
    val paramValue = This(cls).select(cls.fieldMember("param")).asExpr
    Some('{ println("Calling getParam"); $paramValue }.asTerm)
  val getParamDef = DefDef(getParamSym, _ => getParamRhs())

  val clsDef = ClassDef(cls, List(TypeTree.of[Object]), body = List(getParamDef))
  val appliedTypeTree = Applied(TypeIdent(cls), List(TypeTree.of[String]))
  val newCls =
    Typed(
      Apply(
        Select(
          Apply(
            TypeApply(Select(New(appliedTypeTree), cls.primaryConstructor), List(TypeTree.of[String])),
            List(Expr("test").asTerm)
          ),
          cls.methodMember("getParam").head
        ),
        Nil
      ),
      TypeTree.of[String]
    )

  Block(List(clsDef), newCls).asExpr

  // '{
  //   class myClass[T](val param: T) {
  //     def getParam(): T =
  //       println("Calling getParam")
  //       param
  //   }
  //   new myClass[String]("test").getParam()
  // }
}
