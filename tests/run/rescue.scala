object lib {
  inline def (op: => T) rescue[T] (fallback: => T) =
    try op
    catch {
      case _: Throwable => fallback
    }

  inline def (op: => T) rescue[T, E <: Throwable] (fallback: PartialFunction[E, T]) =
    try op
    catch {
      case ex: E =>
        if (fallback.isDefinedAt(ex)) fallback(ex) else throw ex
    }
}

import lib._

@main def Test = {
  assert((9 / 1 rescue 1) == 9)
  assert((9 / 0 rescue 1) == 1)
  assert(((9 / 0 rescue { case ex: NullPointerException => 5  }) rescue 10) == 10)
  assert(((9 / 0 rescue { case ex: ArithmeticException => 5  }) rescue 10) == 5)

  assert(
    {
      9 / 0 rescue {
        case ex: NullPointerException => 4
        case ex: ArithmeticException => 3
      }
    } == 3
  )

  assert(
    {
      {
        val a = 9 / 0 rescue {
          case ex: NullPointerException => 4
        }
        a * a
      } rescue {
        case ex: ArithmeticException => 3
      }
    } == 3
  )
}
