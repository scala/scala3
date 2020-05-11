// Port of https://github.com/liufengyun/gestalt/blob/master/macros/src/main/scala/gestalt/macros/TypeToolbox.scala
// using staging reflection

import scala.quoted._

object TypeToolbox {
  /** are the two types equal? */
  inline def =:=[A, B]: Boolean = ${tpEqImpl('[A], '[B])}
  private def tpEqImpl[A, B](using s: Scope)(a: s.Type[A], b: s.Type[B]): s.Expr[Boolean] =
    Expr(a.tpe =:= b.tpe)

  /** is `tp1` a subtype of `tp2` */
  inline def <:<[A, B]: Boolean = ${tpLEqImpl('[A], '[B])}
  private def tpLEqImpl[A, B](using s: Scope)(a: s.Type[A], b: s.Type[B]): s.Expr[Boolean] =
    Expr(a.tpe <:< b.tpe)

  /** type associated with the tree */
  inline def typeOf[T, Expected](a: T): Boolean = ${typeOfImpl('a, '[Expected])}
  private def typeOfImpl[T, Expected](using s: Scope)(a: s.Expr[T], expected: s.Type[Expected]): s.Expr[Boolean] =
    Expr(a.tpe =:= expected.tpe)

  /** does the type refer to a case class? */
  inline def isCaseClass[A]: Boolean = ${isCaseClassImpl('[A])}
  private def isCaseClassImpl[A](using s: Scope)(tp: s.Type[A]): s.Expr[Boolean] = {
    import s.tasty.Flags
    val sym = tp.symbol
    Expr(sym.isClassDef && sym.flags.is(Flags.Case))
  }

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  inline def caseFields[T]: List[String] = ${caseFieldsImpl('[T])}
  private def caseFieldsImpl[T](using s: Scope)(tp: s.Type[T]): s.Expr[List[String]] = {
    val fields = tp.symbol.caseFields.map(_.name)
    Expr(fields)
  }

  inline def fieldIn[T](inline mem: String): String = ${fieldInImpl('[T], 'mem)}
  private def fieldInImpl[T](using s: Scope)(t: s.Type[T], mem: s.Expr[String]): s.Expr[String] = {
    val field = t.symbol.field(mem.unliftOrError)
    Expr(if field.isNoSymbol then "" else field.name)
  }

  inline def fieldsIn[T]: Seq[String] = ${fieldsInImpl('[T])}
  private def fieldsInImpl[T](using s: Scope)(t: s.Type[T]): s.Expr[Seq[String]] = {
    val fields = t.symbol.fields
    Expr(fields.map(_.name).toList)
  }

  inline def methodIn[T](inline mem: String): Seq[String] = ${methodInImpl('[T], 'mem)}
  private def methodInImpl[T](using s: Scope)(t: s.Type[T], mem: s.Expr[String]): s.Expr[Seq[String]] =
    Expr(t.symbol.classMethod(mem.unliftOrError).map(_.name))

  inline def methodsIn[T]: Seq[String] = ${methodsInImpl('[T])}
  private def methodsInImpl[T](using s: Scope)(t: s.Type[T]): s.Expr[Seq[String]] =
    Expr(t.symbol.classMethods.map(_.name))

  inline def method[T](inline mem: String): Seq[String] = ${methodImpl('[T], 'mem)}
  private def methodImpl[T](using s: Scope)(t: s.Type[T], mem: s.Expr[String]): s.Expr[Seq[String]] =
    Expr(t.symbol.method(mem.unliftOrError).map(_.name))

  inline def methods[T]: Seq[String] = ${methodsImpl('[T])}
  private def methodsImpl[T](using s: Scope)(t: s.Type[T]): s.Expr[Seq[String]] =
    Expr(t.symbol.methods.map(_.name))

  inline def typeTag[T](x: T): String = ${typeTagImpl('[T])}
  private def typeTagImpl[T](using s: Scope)(tp: s.Type[T]): s.Expr[String] = {
    val res = tp.tpe.show
    Expr(res)
  }

  inline def companion[T1, T2]: Boolean = ${companionImpl('[T1], '[T2])}
  private def companionImpl[T1, T2](using s: Scope)(t1: s.Type[T1], t2: s.Type[T2]): s.Expr[Boolean] = {
    val res = t1.symbol.companionModule == t2.symbol
    Expr(res)
  }

  inline def companionName[T1]: String = ${companionNameImpl('[T1])}
  private def companionNameImpl[T1](using s: Scope)(tp: s.Type[T1]): s.Expr[String] = {
    import s.tasty.Symbol
    val sym = tp.symbol
    val companionClass =
      if sym.isClassDef then sym.companionModule.companionClass
      else if sym.isValDef then sym.companionClass
      else Symbol.noSymbol
    Expr(if companionClass.isNoSymbol then "" else companionClass.fullName)
  }

}
