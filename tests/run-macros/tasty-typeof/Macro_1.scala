import scala.quoted._

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._
    '{
      assert(${Expr(Type.of[Unit] =:= defn.UnitType)}, "Unit")
      assert(${Expr(Type.of[Byte] =:= defn.ByteType)}, "Byte")
      assert(${Expr(Type.of[Short] =:= defn.ShortType)}, "Short")
      assert(${Expr(Type.of[Int] =:= defn.IntType)}, "Int")
      assert(${Expr(Type.of[Long] =:= defn.LongType)}, "Long")
      assert(${Expr(Type.of[Float] =:= defn.FloatType)}, "Float")
      assert(${Expr(Type.of[Double] =:= defn.DoubleType)}, "Double")
      assert(${Expr(Type.of[Char] =:= defn.CharType)}, "Char")
      assert(${Expr(Type.of[String] =:= defn.StringType)}, "String")

      assert(${Expr(Type.of[Any] =:= defn.AnyType)}, "Any")
      assert(${Expr(Type.of[AnyRef] =:= defn.AnyRefType)}, "AnyRef")
      assert(${Expr(Type.of[AnyVal] =:= defn.AnyValType)}, "AnyVal")
      assert(${Expr(Type.of[Object] =:= defn.ObjectType)}, "Object")
      assert(${Expr(Type.of[Nothing] =:= defn.NothingType)}, "Nothing")

      println(${Expr(Type.of[List[Int]].show)})
      println(${Expr(Type.of[Macros].show)})
      println(${Expr(Type.of[Macros.type].show)})
    }
  }

}

class Macros
