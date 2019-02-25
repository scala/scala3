import scala.quoted._
import scala.tasty._

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    '{
      assert(${(typeOf[Unit] =:= definitions.UnitType).toExpr}, "Unit")
      assert(${(typeOf[Byte] =:= definitions.ByteType).toExpr}, "Byte")
      assert(${(typeOf[Short] =:= definitions.ShortType).toExpr}, "Short")
      assert(${(typeOf[Int] =:= definitions.IntType).toExpr}, "Int")
      assert(${(typeOf[Long] =:= definitions.LongType).toExpr}, "Long")
      assert(${(typeOf[Float] =:= definitions.FloatType).toExpr}, "Float")
      assert(${(typeOf[Double] =:= definitions.DoubleType).toExpr}, "Double")
      assert(${(typeOf[Char] =:= definitions.CharType).toExpr}, "Char")
      assert(${(typeOf[String] =:= definitions.StringType).toExpr}, "String")

      assert(${(typeOf[Any] =:= definitions.AnyType).toExpr}, "Any")
      assert(${(typeOf[AnyRef] =:= definitions.AnyRefType).toExpr}, "AnyRef")
      assert(${(typeOf[AnyVal] =:= definitions.AnyValType).toExpr}, "AnyVal")
      assert(${(typeOf[Object] =:= definitions.ObjectType).toExpr}, "Object")
      assert(${(typeOf[Nothing] =:= definitions.NothingType).toExpr}, "Nothing")

      println(${typeOf[List[Int]].showCode.toExpr})
      println(${typeOf[Macros].showCode.toExpr})
      println(${typeOf[Macros.type].showCode.toExpr})
    }
  }

}

class Macros
