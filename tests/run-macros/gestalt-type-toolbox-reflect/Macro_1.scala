// Port of https://github.com/liufengyun/gestalt/blob/master/macros/src/main/scala/gestalt/macros/TypeToolbox.scala
// using staging reflection

import scala.quoted._
import scala.tasty._

object TypeToolbox {
  /** are the two types equal? */
  inline def =:=[A, B]: Boolean = ${tpEqImpl('[A], '[B])}
  private def tpEqImpl[A, B](a: Type[A], b: Type[B])(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    val res = a.unseal.tpe =:= b.unseal.tpe
    res.toExpr
  }

  /** is `tp1` a subtype of `tp2` */
  inline def <:<[A, B]: Boolean = ${tpLEqImpl('[A], '[B])}
  private def tpLEqImpl[A, B](a: Type[A], b: Type[B])(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    val res = a.unseal.tpe <:< b.unseal.tpe
    res.toExpr
  }

  /** type associated with the tree */
  inline def typeOf[T, Expected](a: T): Boolean = ${typeOfImpl('a, '[Expected])}
  private def typeOfImpl(a: Expr[_], expected: Type[_])(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    val res = a.unseal.tpe =:= expected.unseal.tpe
    res.toExpr
  }

  /** does the type refer to a case class? */
  inline def isCaseClass[A]: Boolean = ${isCaseClassImpl('[A])}
  private def isCaseClassImpl(tp: Type[_])(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    val res = tp.unseal.symbol match {
      case IsClassDefSymbol(sym) => sym.flags.is(Flags.Case)
      case _ => false
    }
    res.toExpr
  }

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  inline def caseFields[T]: List[String] = ${caseFieldsImpl('[T])}
  private def caseFieldsImpl(tp: Type[_])(implicit reflect: Reflection): Expr[List[String]] = {
    import reflect._
    val fields = tp.unseal.symbol.asClassDef.caseFields.map(_.name)
    fields.toExpr
  }

  inline def fieldIn[T](inline mem: String): String = ${fieldInImpl('[T], mem)}
  private def fieldInImpl(t: Type[_], mem: String)(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    val field = t.unseal.symbol.asClassDef.field(mem)
    field.map(_.name).getOrElse("").toExpr
  }

  inline def fieldsIn[T]: Seq[String] = ${fieldsInImpl('[T])}
  private def fieldsInImpl(t: Type[_])(implicit reflect: Reflection): Expr[Seq[String]] = {
    import reflect._
    val fields = t.unseal.symbol.asClassDef.fields
    fields.map(_.name).toList.toExpr
  }

  inline def methodIn[T](inline mem: String): Seq[String] = ${methodInImpl('[T], mem)}
  private def methodInImpl(t: Type[_], mem: String)(implicit reflect: Reflection): Expr[Seq[String]] = {
    import reflect._
    t.unseal.symbol.asClassDef.classMethod(mem).map(_.name).toExpr
  }

  inline def methodsIn[T]: Seq[String] = ${methodsInImpl('[T])}
  private def methodsInImpl(t: Type[_])(implicit reflect: Reflection): Expr[Seq[String]] = {
    import reflect._
    t.unseal.symbol.asClassDef.classMethods.map(_.name).toExpr
  }

  inline def method[T](inline mem: String): Seq[String] = ${methodImpl('[T], mem)}
  private def methodImpl(t: Type[_], mem: String)(implicit reflect: Reflection): Expr[Seq[String]] = {
    import reflect._
    t.unseal.symbol.asClassDef.method(mem).map(_.name).toExpr
  }

  inline def methods[T]: Seq[String] = ${methodsImpl('[T])}
  private def methodsImpl(t: Type[_])(implicit reflect: Reflection): Expr[Seq[String]] = {
    import reflect._
    t.unseal.symbol.asClassDef.methods.map(_.name).toExpr
  }

  inline def typeTag[T](x: T): String = ${typeTagImpl('[T])}
  private def typeTagImpl(tp: Type[_])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    val res = tp.unseal.tpe.show
    res.toExpr
  }

  inline def companion[T1, T2]: Boolean = ${companionImpl('[T1], '[T2])}
  private def companionImpl(t1: Type[_], t2: Type[_])(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    val res = t1.unseal.symbol.asClassDef.companionModule.contains(t2.unseal.symbol)
    res.toExpr
  }

  inline def companionName[T1]: String = ${companionNameImpl('[T1])}
  private def companionNameImpl(tp: Type[_])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    val companionClassOpt = tp.unseal.symbol match {
      case IsClassDefSymbol(sym) => sym.companionClass
      case IsValDefSymbol(sym) => sym.companionClass
      case _ => None
    }
    companionClassOpt.map(_.fullName).getOrElse("").toExpr
  }

  // TODO add to the std lib
  private implicit def listIsLiftable[T: Type: Liftable]: Liftable[List[T]] = new Liftable {
    def toExpr(list: List[T]): Expr[List[T]] = list match {
      case x :: xs => '{${x.toExpr} :: ${toExpr(xs)}}
      case Nil => '{Nil}
    }
  }
}
