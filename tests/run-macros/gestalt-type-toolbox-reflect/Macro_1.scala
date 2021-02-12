// Port of https://github.com/liufengyun/gestalt/blob/master/macros/src/main/scala/gestalt/macros/TypeToolbox.scala
// using staging reflection

import scala.quoted.*

object TypeToolbox {
  /** are the two types equal? */
  inline def =:=[A, B]: Boolean = ${tpEqImpl[A, B]}
  private def tpEqImpl[A: Type, B: Type](using Quotes) : Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[A] =:= TypeRepr.of[B])
  }

  /** is `tp1` a subtype of `tp2` */
  inline def <:<[A, B]: Boolean = ${tpLEqImpl[A, B]}
  private def tpLEqImpl[A: Type, B: Type](using Quotes) : Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[A] <:< TypeRepr.of[B])
  }

  /** type associated with the tree */
  inline def typeOf[T, Expected](a: T): Boolean = ${typeOfImpl[T, Expected]('a)}
  private def typeOfImpl[A: Type, E: Type](a: Expr[A])(using Quotes) : Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[A] =:= TypeRepr.of[E])
  }

  /** does the type refer to a case class? */
  inline def isCaseClass[A]: Boolean = ${isCaseClassImpl[A]}
  private def isCaseClassImpl[T: Type](using Quotes) : Expr[Boolean] = {
    import quotes.reflect.*
    val sym = TypeTree.of[T].symbol
    Expr(sym.isClassDef && sym.flags.is(Flags.Case))
  }

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  inline def caseFields[T]: List[String] = ${caseFieldsImpl[T]}
  private def caseFieldsImpl[T: Type](using Quotes) : Expr[List[String]] = {
    import quotes.reflect.*
    val fields = TypeTree.of[T].symbol.caseFields.map(_.name)
    Expr(fields)
  }

  inline def fieldIn[T](inline mem: String): String = ${fieldInImpl[T]('mem)}
  private def fieldInImpl[T: Type](mem: Expr[String])(using Quotes) : Expr[String] = {
    import quotes.reflect.*
    val field = TypeTree.of[T].symbol.declaredField(mem.valueOrError)
    Expr(if field.isNoSymbol then "" else field.name)
  }

  inline def fieldsIn[T]: Seq[String] = ${fieldsInImpl[T]}
  private def fieldsInImpl[T: Type](using Quotes) : Expr[Seq[String]] = {
    import quotes.reflect.*
    val fields = TypeTree.of[T].symbol.declaredFields
    Expr(fields.map(_.name).toList)
  }

  inline def methodIn[T](inline mem: String): Seq[String] = ${methodInImpl[T]('mem)}
  private def methodInImpl[T: Type](mem: Expr[String])(using Quotes) : Expr[Seq[String]] = {
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.declaredMethod(mem.valueOrError).map(_.name))
  }

  inline def methodsIn[T]: Seq[String] = ${methodsInImpl[T]}
  private def methodsInImpl[T: Type](using Quotes) : Expr[Seq[String]] = {
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.declaredMethods.map(_.name))
  }

  inline def method[T](inline mem: String): Seq[String] = ${methodImpl[T]('mem)}
  private def methodImpl[T: Type](mem: Expr[String])(using Quotes) : Expr[Seq[String]] = {
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.memberMethod(mem.valueOrError).map(_.name))
  }

  inline def methods[T]: Seq[String] = ${methodsImpl[T]}
  private def methodsImpl[T: Type](using Quotes) : Expr[Seq[String]] = {
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.memberMethods.map(_.name))
  }

  inline def typeTag[T](x: T): String = ${typeTagImpl[T]}
  private def typeTagImpl[T: Type](using Quotes) : Expr[String] = {
    import quotes.reflect.*
    val res = TypeRepr.of[T].show
    Expr(res)
  }

  inline def companion[T1, T2]: Boolean = ${companionImpl[T1, T2]}
  private def companionImpl[T1: Type, T2: Type](using Quotes) : Expr[Boolean] = {
    import quotes.reflect.*
    val res = TypeTree.of[T1].symbol.companionModule == TypeTree.of[T2].symbol
    Expr(res)
  }

  inline def companionName[T1]: String = ${companionNameImpl[T1]}
  private def companionNameImpl[T: Type](using Quotes) : Expr[String] = {
    import quotes.reflect.*
    val sym = TypeTree.of[T].symbol
    val companionClass =
      if sym.isClassDef then sym.companionModule.companionClass
      else if sym.isValDef then sym.companionClass
      else Symbol.noSymbol
    Expr(if companionClass.isNoSymbol then "" else companionClass.fullName)
  }

}
