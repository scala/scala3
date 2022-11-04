import language.experimental.saferExceptions

class LimitExceeded extends Exception

val limit = 10e+10

def f(x: Double): Double throws LimitExceeded =
  if x < limit then x * x else throw LimitExceeded()

def escaped(xs: Double*)(using CanThrow[LimitExceeded]): () => Double =
  try () => xs.map(f).sum  // error
  catch case ex: LimitExceeded => () => -1

def escaped2(xs: Double*): (() => Double) throws LimitExceeded =
  try () => xs.map(f).sum  // error
  catch case ex: LimitExceeded => () => -1
