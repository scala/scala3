import scala.quoted._

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._
    '{
      assert(${Expr(typeOf[Unit] =:= defn.UnitType)}, "Unit")
      assert(${Expr(typeOf[Byte] =:= defn.ByteType)}, "Byte")
      assert(${Expr(typeOf[Short] =:= defn.ShortType)}, "Short")
      assert(${Expr(typeOf[Int] =:= defn.IntType)}, "Int")
      assert(${Expr(typeOf[Long] =:= defn.LongType)}, "Long")
      assert(${Expr(typeOf[Float] =:= defn.FloatType)}, "Float")
      assert(${Expr(typeOf[Double] =:= defn.DoubleType)}, "Double")
      assert(${Expr(typeOf[Char] =:= defn.CharType)}, "Char")
      assert(${Expr(typeOf[String] =:= defn.StringType)}, "String")

      assert(${Expr(typeOf[Any] =:= defn.AnyType)}, "Any")
      assert(${Expr(typeOf[AnyRef] =:= defn.AnyRefType)}, "AnyRef")
      assert(${Expr(typeOf[AnyVal] =:= defn.AnyValType)}, "AnyVal")
      assert(${Expr(typeOf[Object] =:= defn.ObjectType)}, "Object")
      assert(${Expr(typeOf[Nothing] =:= defn.NothingType)}, "Nothing")

      println(${Expr(typeOf[List[Int]].show)})
      println(${Expr(typeOf[Macros].show)})
      println(${Expr(typeOf[Macros.type].show)})
    }
  }

}

class Macros
