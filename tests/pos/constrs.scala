class Foo(x: Int, var y: Int) {

  val z: Int = 0

  var u: Int = compiletime.uninitialized

  def f = x

}

class Baz(val base: Int) {

}


class Bar(base: Int, byName: => String, local: Int) extends Baz(base + local) {

  def f() = println(base.toString + byName)

}

class Rational(n: Int, d: Int) {
  def gcd(x: Int, y: Int): Int = ???
  private val x = gcd(n, d)
  def numer = n / x
  def denom = d / x
}
class Rational2(n: Int, d: Int) {
  def gcd(x: Int, y: Int): Int = ???
  private val x = gcd(n, d)
  val numer = n / x
  val denom = d / x
}
