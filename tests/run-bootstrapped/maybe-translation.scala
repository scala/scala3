//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.{Ok, Err}

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
                    case val x4: Int = Int.unbox(x1:Object)
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
    def baz(x: Object): Object =
      (boundary2[Object]:
        {
          val y: Int =

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
              val x$proxy1: Object = scala.withErr(x, "not an int")
              matchResult2[Int]:
                {
                  case val x6: Object = x$proxy1
                  if
                    (null == x6).unary_!() &&
                      x6.isInstanceOf[scala.runtime.Fail].unary_!()
                   then
                    {
                      case val x9: Int = Int.unbox(x6:Object)
                      case val y: Int = x9
                      return[matchResult2] y:Int
                    }
                   else ()
                  {
                    case val x7: Object =
                      x6.asInstanceOf[scala.runtime.Fail].elem()
                    case val e: String = x7.asInstanceOf[String]
                    return[matchResult2]
                      return[boundary2]
                        new scala.runtime.Fail(e):Object:Object
                  }
                  throw new MatchError(x6)
                }
            }
          if (y > 0).unary_!() then
            return[boundary2]
              new scala.runtime.Fail("not positive"):Object:Object
           else ()
          Int.box(y:Int)
        }
      ):Object:Object

*/

def maybeReverse(s: String?): String? = s match
  case Ok(s) => s.reverse
  case null => null

/** With -Vprint:erasure should produce something like:

      matchResult3[String]:
        {
          case val x11: String = s
          if (null == x11).unary_!() then
            {
              case val x12: String = x11.asInstanceOf[String]
              case val s: String = x12
              return[matchResult3]
                {
                  scala.collection.StringOps.reverse$extension(augmentString(s))
                    :String
                }
            }
           else ()
          return[matchResult3]
            {
              null
            }
          throw new MatchError(x11)
        }
*/
@main def Test =
  assert(foo(6) == 6)
  assert(foo(11) == null)
  assert(bar(6) == 6)
  assert(bar(-1) == null)
  assert(baz(6) == 6)
  assert(baz(-1) == Err("not positive"), baz(-1))
