import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

object Macros {

  inline def testDefinitions(): Unit = ${testDefinitionsImpl}

  def testDefinitionsImpl(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val buff = List.newBuilder[String]
    def printout(x: => String): Unit = {

      buff += (try x catch { case ex => ex.getClass + ": " + ex.getMessage})
    }

    printout(definitions.RootPackage.name)
    printout(definitions.ScalaPackage.name)

    printout(definitions.AnyClass.name)
    printout(definitions.AnyValClass.name)
    printout(definitions.ObjectClass.name)
    printout(definitions.AnyRefClass.name)

    printout(definitions.NullClass.name)
    printout(definitions.NothingClass.name)

    printout(definitions.UnitClass.name)
    printout(definitions.ByteClass.name)
    printout(definitions.ShortClass.name)
    printout(definitions.CharClass.name)
    printout(definitions.IntClass.name)
    printout(definitions.LongClass.name)
    printout(definitions.FloatClass.name)
    printout(definitions.DoubleClass.name)
    printout(definitions.BooleanClass.name)

    printout(definitions.StringClass.name)
    printout(definitions.ClassClass.name)
    printout(definitions.ArrayClass.name)
    printout(definitions.PredefModule.name)

    printout(definitions.JavaLangPackage.name)

    printout(definitions.ArrayModule.name)

    printout(definitions.Array_apply.name)
    printout(definitions.Array_clone.name)
    printout(definitions.Array_length.name)
    printout(definitions.Array_update.name)

    printout(definitions.RepeatedParamClass.name)

    printout(definitions.OptionClass.name)
    printout(definitions.NoneModule.name)
    printout(definitions.SomeModule.name)

    printout(definitions.ProductClass.name)

    for (i <- 0 to 25)
      printout(definitions.FunctionClass(i).name)

    for (i <- 0 to 25)
      printout(definitions.FunctionClass(i, isImplicit = true).name)

    for (i <- 1 to 25)
      printout(definitions.FunctionClass(i, isErased = true).name)

    for (i <- 1 to 25)
      printout(definitions.FunctionClass(i, isImplicit = true, isErased = true).name)

    for (i <- 2 to 22)
      printout(definitions.TupleClass(i).name)

    printout(definitions.ScalaPrimitiveValueClasses.map(_.name).toString)
    printout(definitions.ScalaNumericValueClasses.map(_.name).toString)

    printout(definitions.UnitType.showExtractors)
    printout(definitions.ByteType.showExtractors)
    printout(definitions.CharType.showExtractors)
    printout(definitions.IntType.showExtractors)
    printout(definitions.LongType.showExtractors)
    printout(definitions.FloatType.showExtractors)
    printout(definitions.DoubleType.showExtractors)
    printout(definitions.BooleanType.showExtractors)
    printout(definitions.AnyType.showExtractors)
    printout(definitions.AnyValType.showExtractors)
    printout(definitions.AnyRefType.showExtractors)
    printout(definitions.ObjectType.showExtractors)
    printout(definitions.NothingType.showExtractors)
    printout(definitions.NullType.showExtractors)
    printout(definitions.StringType.showExtractors)


    '{println(${buff.result().mkString("\n")})}
  }

}
