import Conversion.convert

given Conversion[String, Int] = _.length
given Conversion[Int, String] = _.toString

def f(x: String): Int = x.convert
def g(x: Int): String = x.convert[String]
