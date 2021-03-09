import Conversion.into

given Conversion[String, Int] = _.length
given Conversion[Int, String] = _.toString

def f(x: String): Int = x.into
def g(x: Int): String = x.into[String]
