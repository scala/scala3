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

    printout(definitions.UnitType.show)
    printout(definitions.ByteType.show)
    printout(definitions.CharType.show)
    printout(definitions.IntType.show)
    printout(definitions.LongType.show)
    printout(definitions.FloatType.show)
    printout(definitions.DoubleType.show)
    printout(definitions.BooleanType.show)
    printout(definitions.AnyType.show)
    printout(definitions.AnyValType.show)
    printout(definitions.AnyRefType.show)
    printout(definitions.ObjectType.show)
    printout(definitions.NothingType.show)
    printout(definitions.NullType.show)
    printout(definitions.StringType.show)


    '{println(${buff.result().mkString("\n")})}
  }

}
