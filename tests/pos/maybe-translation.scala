//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*

def foo(x: Int): Int? =
  if x > 0 && x < 10 then x else null

def bar(x: Int?) =
  maybe:
    val y = x?
    if y > 0
    y

/* With -Vprint:erasure should produce something like:

      (boundary1[Object]:
        {
          val y: Int =
            matchResult1[Int]:
              {
                case val x1: Object = x
                if (null == x1).unary_!() then
                  {
                    case val x4: Int =
                      Int.unbox(
                        (if x1.isInstanceOf[scala.magic.runtime.Valid] then
                          x1.asInstanceOf[scala.magic.runtime.Valid].elem()
                           else x1):Object
                      )
                    case val y: Int = x4
                    return[matchResult1] y:Int
                  }
                 else ()
                {
                  case val x3: scala.runtime.BoxedUnit =
                    scala.runtime.BoxedUnit.UNIT
                  case val e: scala.runtime.BoxedUnit = x3
                  return[matchResult1] return[boundary1] null:Object:Object
                }
                throw new MatchError(x1)
              }
          if (y > 0).unary_!() then return[boundary1] null:Object:Object else ()
          Int.box(y:Int)
        }
      ):Object:Object
*/

def baz(x: Int ? String) =
  maybe:
    val y = x.withErr("not an int")?
    if y > 0 else "not positive"
    y

/* With -Vprint:erasure should produce something like:

      (boundary2[Object]:
        {
          val y: Int =
            {
              val x$proxy1: Object = scala.magic.withErr(x, "not an int")
              matchResult2[Int]:
                {
                  case val x6: Object = x$proxy1
                  if
                    (null == x6).unary_!() &&
                      x6.isInstanceOf[scala.magic.runtime.Fail].unary_!()
                   then
                    {
                      case val x9: Int =
                        Int.unbox(
                          (if x6.isInstanceOf[scala.magic.runtime.Valid] then
                            x6.asInstanceOf[scala.magic.runtime.Valid].elem()
                             else x6):Object
                        )
                      case val y: Int = x9
                      return[matchResult2] y:Int
                    }
                   else ()
                  {
                    case val x7: Object =
                      x6.asInstanceOf[scala.magic.runtime.Fail].elem()
                    case val e: String = x7.asInstanceOf[String]
                    return[matchResult2]
                      return[boundary2]
                        new scala.magic.runtime.Fail(e):Object:Object
                  }
                  throw new MatchError(x6)
                }
            }
          if (y > 0).unary_!() then
            return[boundary2]
              new scala.magic.runtime.Fail("not positive"):Object:Object
           else ()
          Int.box(y:Int)
        }
      ):Object:Object
*/
