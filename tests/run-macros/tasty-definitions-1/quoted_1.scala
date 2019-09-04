import scala.quoted._
import given scala.quoted.autolift._

object Macros {

  inline def testDefinitions(): Unit = ${testDefinitionsImpl}

  def testDefinitionsImpl given (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

    val buff = List.newBuilder[String]
    def printout(x: => String): Unit = {

      buff += (try x catch { case ex => ex.getClass + ": " + ex.getMessage})
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

    printout(defn.UnitType.showExtractors)
    printout(defn.ByteType.showExtractors)
    printout(defn.CharType.showExtractors)
    printout(defn.IntType.showExtractors)
    printout(defn.LongType.showExtractors)
    printout(defn.FloatType.showExtractors)
    printout(defn.DoubleType.showExtractors)
    printout(defn.BooleanType.showExtractors)
    printout(defn.AnyType.showExtractors)
    printout(defn.AnyValType.showExtractors)
    printout(defn.AnyRefType.showExtractors)
    printout(defn.ObjectType.showExtractors)
    printout(defn.NothingType.showExtractors)
    printout(defn.NullType.showExtractors)
    printout(defn.StringType.showExtractors)


    '{println(${buff.result().mkString("\n")})}
  }

}
