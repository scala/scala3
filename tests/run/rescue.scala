object lib {
  inline def (op: => T) rescue[T] (fallback: => T) = try op catch { case _: Throwable => fallback }
  inline def (op: => T) rescue[T, E <: Throwable] (fallback: E => T) = try op catch { case ex: E => fallback(ex) }
}

import lib._

@main def Test = {
  assert((9 / 1 rescue 1) == 9)
  assert((9 / 0 rescue 1) == 1)
  assert(((9 / 0 rescue { ex: NullPointerException => 5  }) rescue 10) == 10)
  assert(((9 / 0 rescue { ex: ArithmeticException => 5  }) rescue 10) == 5)
}
