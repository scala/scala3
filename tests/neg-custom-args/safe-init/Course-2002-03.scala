object M2 {
  class Rational(x: Int, y: Int) {
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b);
    private val g = gcd(x, y);
    def numer = x / g;
    def denom = y / g;
    def add(r: Rational) =
      new Rational(
        numer * r.denom + r.numer * denom,
        denom * r.denom);
    def sub(r: Rational) =
      new Rational(
        numer * r.denom - r.numer * denom,
        denom * r.denom);
    def mul(r: Rational) =
      new Rational(
        numer * r.numer,
        denom * r.denom);
    def div(r: Rational) =
      new Rational(
        numer * r.denom,
        denom * r.numer);
    override def toString() = numer + "/" + denom;
  }

  val x = new Rational(1, 3);
  val y = new Rational(5, 7);
  val z = new Rational(3, 2);
  Console.println(x);
  Console.println(y);
  Console.println(z);
  Console.println(x.add(y).mul(z));
  Console.println();

  val m = n         // error
  val n: Int = 10
}
