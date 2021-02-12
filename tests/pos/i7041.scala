import scala.util.control.NonLocalReturns.*

extension [T, E <: Throwable](op: => T)
  inline def rescue (fallback: PartialFunction[E, T]) =
    try op
    catch {
      case ex: ReturnThrowable[_] => throw ex
      case ex: E =>
        if (fallback.isDefinedAt(ex))  fallback(ex) else throw ex
    }

def test: Unit = {
  9 / 0 rescue { case _: ArithmeticException => 10 }
}