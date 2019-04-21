import scala.quoted._
import scala.quoted.autolift._
import scala.tasty._

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    '{
      assert(${(typeOf[Unit] =:= definitions.UnitType)}, "Unit")
      assert(${(typeOf[Byte] =:= definitions.ByteType)}, "Byte")
      assert(${(typeOf[Short] =:= definitions.ShortType)}, "Short")
      assert(${(typeOf[Int] =:= definitions.IntType)}, "Int")
      assert(${(typeOf[Long] =:= definitions.LongType)}, "Long")
      assert(${(typeOf[Float] =:= definitions.FloatType)}, "Float")
      assert(${(typeOf[Double] =:= definitions.DoubleType)}, "Double")
      assert(${(typeOf[Char] =:= definitions.CharType)}, "Char")
      assert(${(typeOf[String] =:= definitions.StringType)}, "String")

      assert(${(typeOf[Any] =:= definitions.AnyType)}, "Any")
      assert(${(typeOf[AnyRef] =:= definitions.AnyRefType)}, "AnyRef")
      assert(${(typeOf[AnyVal] =:= definitions.AnyValType)}, "AnyVal")
      assert(${(typeOf[Object] =:= definitions.ObjectType)}, "Object")
      assert(${(typeOf[Nothing] =:= definitions.NothingType)}, "Nothing")

      println(${typeOf[List[Int]].show})
      println(${typeOf[Macros].show})
      println(${typeOf[Macros.type].show})
    }
  }

}

class Macros
