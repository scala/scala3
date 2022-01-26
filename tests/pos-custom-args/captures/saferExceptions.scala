import language.experimental.saferExceptions

class LimitExceeded extends Exception

val limit = 10e9

extension [A](xs: Seq[A])
  def mapp[B](f: A => B): Seq[B] =
    xs.map(f.asInstanceOf[A -> B])

def f(x: Double): Double throws LimitExceeded =
  if x < limit then x * x else throw LimitExceeded()

@main def test(xs: Double*) =
  try println(xs.mapp(f).sum + xs.map(f).sum)
  catch case ex: LimitExceeded => println("too large")


