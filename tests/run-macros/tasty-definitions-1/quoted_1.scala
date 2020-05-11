import scala.quoted._

object Macros {

  inline def testDefinitions(): Unit = ${testDefinitionsImpl}

  def testDefinitionsImpl(using s: Scope): s.Expr[Unit] = {
    import s.tasty._

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

    printout(Type.of[Unit].showExtractors)
    printout(Type.of[Byte].showExtractors)
    printout(Type.of[Char].showExtractors)
    printout(Type.of[Int].showExtractors)
    printout(Type.of[Long].showExtractors)
    printout(Type.of[Float].showExtractors)
    printout(Type.of[Double].showExtractors)
    printout(Type.of[Boolean].showExtractors)
    printout(Type.of[Any].showExtractors)
    printout(Type.of[AnyVal].showExtractors)
    printout(Type.of[AnyRef].showExtractors)
    printout(Type.of[Object].showExtractors)
    printout(Type.of[Nothing].showExtractors)
    printout(Type.of[Null].showExtractors)
    printout(Type.of[String].showExtractors)


    '{println(${Expr(buff.result().mkString("\n"))})}
  }

}
