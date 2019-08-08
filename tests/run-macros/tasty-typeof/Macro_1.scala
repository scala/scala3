import scala.quoted._
import given scala.quoted.autolift._

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl given (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._
    '{
      assert(${(typeOf[Unit] =:= defn.UnitType)}, "Unit")
      assert(${(typeOf[Byte] =:= defn.ByteType)}, "Byte")
      assert(${(typeOf[Short] =:= defn.ShortType)}, "Short")
      assert(${(typeOf[Int] =:= defn.IntType)}, "Int")
      assert(${(typeOf[Long] =:= defn.LongType)}, "Long")
      assert(${(typeOf[Float] =:= defn.FloatType)}, "Float")
      assert(${(typeOf[Double] =:= defn.DoubleType)}, "Double")
      assert(${(typeOf[Char] =:= defn.CharType)}, "Char")
      assert(${(typeOf[String] =:= defn.StringType)}, "String")

      assert(${(typeOf[Any] =:= defn.AnyType)}, "Any")
      assert(${(typeOf[AnyRef] =:= defn.AnyRefType)}, "AnyRef")
      assert(${(typeOf[AnyVal] =:= defn.AnyValType)}, "AnyVal")
      assert(${(typeOf[Object] =:= defn.ObjectType)}, "Object")
      assert(${(typeOf[Nothing] =:= defn.NothingType)}, "Nothing")

      println(${typeOf[List[Int]].show})
      println(${typeOf[Macros].show})
      println(${typeOf[Macros.type].show})
    }
  }

}

class Macros
