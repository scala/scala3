import scala.quoted.*

class ForeignWrapper1[-A] {
  inline def getTypeInfo(inline source: String): String =
    ${ getTypeInfoImpl[A]('source) }
  def createWrapper2 = ForeignWrapper2(this)
}

class ForeignWrapper2[-A](val self: ForeignWrapper1[A]) {
  inline def getTypeInfo(inline source: String): String =
    ${getTypeInfoImpl[A]('source)}
}

transparent inline def getTypeInfo[T](inline source: String) =
  ${ getTypeInfoImpl[T]('source) }

def getTypeInfoImpl[T: Type](source: Expr[String])(using ctx: Quotes) : Expr[String] = {
  import ctx.reflect.*

  val tpe = TypeRepr.of[T]
  val str =
    s"""|------ ${source.valueOrAbort} -------
       |Original: ${tpe.show}
       |Dealias: ${tpe.dealias.show}
       |Dealias dealias: ${tpe.dealias.dealias.show}
    """.stripMargin

  Expr(str)
}