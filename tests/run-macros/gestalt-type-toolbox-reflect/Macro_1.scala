// Port of https://github.com/liufengyun/gestalt/blob/master/macros/src/main/scala/gestalt/macros/TypeToolbox.scala
// using staging reflection

import scala.quoted._

object TypeToolbox {
  /** are the two types equal? */
  inline def =:=[A, B]: Boolean = ${tpEqImpl('[A], '[B])}
  private def tpEqImpl[A, B](a: Type[A], b: Type[B])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty._
    Expr(a.unseal.tpe =:= b.unseal.tpe)
  }

  /** is `tp1` a subtype of `tp2` */
  inline def <:<[A, B]: Boolean = ${tpLEqImpl('[A], '[B])}
  private def tpLEqImpl[A, B](a: Type[A], b: Type[B])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty._
    Expr(a.unseal.tpe <:< b.unseal.tpe)
  }

  /** type associated with the tree */
  inline def typeOf[T, Expected](a: T): Boolean = ${typeOfImpl('a, '[Expected])}
  private def typeOfImpl(a: Expr[_], expected: Type[_])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty._
    Expr(a.unseal.tpe =:= expected.unseal.tpe)
  }

  /** does the type refer to a case class? */
  inline def isCaseClass[A]: Boolean = ${isCaseClassImpl('[A])}
  private def isCaseClassImpl(tp: Type[_])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty._
    val sym = tp.unseal.symbol
    Expr(sym.isClassDef && sym.flags.is(Flags.Case))
  }

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  inline def caseFields[T]: List[String] = ${caseFieldsImpl('[T])}
  private def caseFieldsImpl(tp: Type[_])(given qctx: QuoteContext): Expr[List[String]] = {
    import qctx.tasty._
    val fields = tp.unseal.symbol.asClassDef.caseFields.map(_.name)
    Expr(fields)
  }

  inline def fieldIn[T](inline mem: String): String = ${fieldInImpl('[T], mem)}
  private def fieldInImpl(t: Type[_], mem: String)(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    val field = t.unseal.symbol.asClassDef.field(mem)
    Expr(if field.isNoSymbol then "" else field.name)
  }

  inline def fieldsIn[T]: Seq[String] = ${fieldsInImpl('[T])}
  private def fieldsInImpl(t: Type[_])(given qctx: QuoteContext): Expr[Seq[String]] = {
    import qctx.tasty._
    val fields = t.unseal.symbol.asClassDef.fields
    Expr(fields.map(_.name).toList)
  }

  inline def methodIn[T](inline mem: String): Seq[String] = ${methodInImpl('[T], mem)}
  private def methodInImpl(t: Type[_], mem: String)(given qctx: QuoteContext): Expr[Seq[String]] = {
    import qctx.tasty._
    Expr(t.unseal.symbol.asClassDef.classMethod(mem).map(_.name))
  }

  inline def methodsIn[T]: Seq[String] = ${methodsInImpl('[T])}
  private def methodsInImpl(t: Type[_])(given qctx: QuoteContext): Expr[Seq[String]] = {
    import qctx.tasty._
    Expr(t.unseal.symbol.asClassDef.classMethods.map(_.name))
  }

  inline def method[T](inline mem: String): Seq[String] = ${methodImpl('[T], mem)}
  private def methodImpl(t: Type[_], mem: String)(given qctx: QuoteContext): Expr[Seq[String]] = {
    import qctx.tasty._
    Expr(t.unseal.symbol.asClassDef.method(mem).map(_.name))
  }

  inline def methods[T]: Seq[String] = ${methodsImpl('[T])}
  private def methodsImpl(t: Type[_])(given qctx: QuoteContext): Expr[Seq[String]] = {
    import qctx.tasty._
    Expr(t.unseal.symbol.asClassDef.methods.map(_.name))
  }

  inline def typeTag[T](x: T): String = ${typeTagImpl('[T])}
  private def typeTagImpl(tp: Type[_])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    val res = tp.unseal.tpe.show
    Expr(res)
  }

  inline def companion[T1, T2]: Boolean = ${companionImpl('[T1], '[T2])}
  private def companionImpl(t1: Type[_], t2: Type[_])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty._
    val res = t1.unseal.symbol.asClassDef.companionModule == t2.unseal.symbol
    Expr(res)
  }

  inline def companionName[T1]: String = ${companionNameImpl('[T1])}
  private def companionNameImpl(tp: Type[_])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    val sym = tp.unseal.symbol
    val companionClass =
      if sym.isClassDef then sym.companionModule.companionClass
      else if sym.isValDef then sym.companionClass
      else Symbol.noSymbol
    Expr(if companionClass.isNoSymbol then "" else companionClass.fullName)
  }

}
