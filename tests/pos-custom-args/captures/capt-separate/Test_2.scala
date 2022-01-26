import language.experimental.saferExceptions
import Lib.*

class LimitExceeded extends Exception

val limit = 10e9

def f(x: Double): Double throws LimitExceeded =
  if x < limit then x * x else throw LimitExceeded()

@main def test(xs: Double*) =
  try println(xs.mapp(f).sum)
  catch case ex: LimitExceeded => println("too large")


