import scala.util.control.NonLocalReturns._

inline def (op: => T) rescue[T, E <: Throwable] (fallback: PartialFunction[E, T]) =
  try op
  catch {
    case ex: ReturnThrowable[_] => throw ex
    case ex: E =>
      if (fallback.isDefinedAt(ex))  fallback(ex) else throw ex
  }

def test: Unit = {
  9 / 0 rescue { case _: ArithmeticException => 10 }
}