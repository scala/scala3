//> using options -experimental

import scala.quoted.*
import scala.annotation.StaticAnnotation

class ScalaAnnotation(value: String) extends StaticAnnotation

inline def makeClass(inline name: String): Any = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Any] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  def decls(cls: Symbol): List[Symbol] = Nil
  val conMethodType = (classType: TypeRepr) => MethodType(Nil)((_: MethodType) => Nil, (_: MethodType) => classType)

  val javaAnnotSym = TypeRepr.of[JavaAnnot].typeSymbol
  val scalaAnnotSym = TypeRepr.of[ScalaAnnotation].typeSymbol

  val javaAnnotationDef = Apply(Select(New(TypeIdent(javaAnnotSym)), javaAnnotSym.primaryConstructor), List(Literal(StringConstant("string in a java annotation"))))
  val scalaAnnotationDef = Apply(Select(New(TypeIdent(scalaAnnotSym)), scalaAnnotSym.primaryConstructor), List(Literal(StringConstant("string in a scala annotation"))))

  val cls = Symbol.newClass(
    Symbol.spliceOwner,
    name,
    parents = _ => List(TypeRepr.of[Object]),
    decls,
    selfType = None,
    clsFlags = Flags.EmptyFlags,
    clsPrivateWithin = Symbol.noSymbol,
    clsAnnotations = List(javaAnnotationDef, scalaAnnotationDef),
    conMethodType,
    conFlags = Flags.EmptyFlags,
    conPrivateWithin = Symbol.noSymbol,
    conParamFlags = List(List()),
    conParamPrivateWithins = List(List())
  )

  val clsDef = ClassDef(cls, List(TypeTree.of[Object]), body = Nil)
  val newCls =
    Typed(
      Apply(
        Select(New(TypeIdent(cls)), cls.primaryConstructor),
        Nil
      ),
      TypeTree.of[Any]
    )

  val res = Block(List(clsDef), newCls).asExpr

  Expr.ofTuple(res, Expr(res.show))

  // {
  //   @JavaAnnot("string in a java annotation") @ScalaAnnotation("string in a scala annotation") class name() extends java.lang.Object
  //   (new name(): scala.Any)
  // }
}
