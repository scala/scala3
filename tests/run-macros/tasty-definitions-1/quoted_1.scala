import scala.quoted._

object Macros {

  inline def testDefinitions(): Unit = ${testDefinitionsImpl}

  def testDefinitionsImpl(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val buff = List.newBuilder[String]
    def printout(x: => String): Unit = {

      buff += (try x catch { case ex => ex.getClass.toString + ": " + ex.getMessage})
    }

    printout(defn.RootPackage.name)
    printout(defn.ScalaPackage.name)

    printout(defn.AnyClass.name)
    printout(defn.AnyValClass.name)
    printout(defn.ObjectClass.name)
    printout(defn.AnyRefClass.name)

    printout(defn.NullClass.name)
    printout(defn.NothingClass.name)

    printout(defn.UnitClass.name)
    printout(defn.ByteClass.name)
    printout(defn.ShortClass.name)
    printout(defn.CharClass.name)
    printout(defn.IntClass.name)
    printout(defn.LongClass.name)
    printout(defn.FloatClass.name)
    printout(defn.DoubleClass.name)
    printout(defn.BooleanClass.name)

    printout(defn.StringClass.name)
    printout(defn.ClassClass.name)
    printout(defn.ArrayClass.name)
    printout(defn.PredefModule.name)

    printout(defn.JavaLangPackage.name)

    printout(defn.ArrayModule.name)

    printout(defn.Array_apply.name)
    printout(defn.Array_clone.name)
    printout(defn.Array_length.name)
    printout(defn.Array_update.name)

    printout(defn.RepeatedParamClass.name)

    printout(defn.OptionClass.name)
    printout(defn.NoneModule.name)
    printout(defn.SomeModule.name)

    printout(defn.ProductClass.name)

    for (i <- 0 to 25)
      printout(defn.FunctionClass(i).name)

    for (i <- 0 to 25)
      printout(defn.FunctionClass(i, isImplicit = true).name)

    for (i <- 1 to 25)
      printout(defn.FunctionClass(i, isErased = true).name)

    for (i <- 1 to 25)
      printout(defn.FunctionClass(i, isImplicit = true, isErased = true).name)

    for (i <- 2 to 22)
      printout(defn.TupleClass(i).name)

    printout(defn.ScalaPrimitiveValueClasses.map(_.name).toString)
    printout(defn.ScalaNumericValueClasses.map(_.name).toString)

    printout(TypeRepr.of[Unit].showExtractors)
    printout(TypeRepr.of[Byte].showExtractors)
    printout(TypeRepr.of[Char].showExtractors)
    printout(TypeRepr.of[Int].showExtractors)
    printout(TypeRepr.of[Long].showExtractors)
    printout(TypeRepr.of[Float].showExtractors)
    printout(TypeRepr.of[Double].showExtractors)
    printout(TypeRepr.of[Boolean].showExtractors)
    printout(TypeRepr.of[Any].showExtractors)
    printout(TypeRepr.of[AnyVal].showExtractors)
    printout(TypeRepr.of[AnyRef].showExtractors)
    printout(TypeRepr.of[Object].showExtractors)
    printout(TypeRepr.of[Nothing].showExtractors)
    printout(TypeRepr.of[Null].showExtractors)
    printout(TypeRepr.of[String].showExtractors)


    '{println(${Expr(buff.result().mkString("\n"))})}
  }

}
