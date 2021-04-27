import Macros.*

object Test {

  def main(args: Array[String]): Unit = {

    println(lift[[X] =>> String](new Show)(3))
    println(lift[[X] =>> X](new Eval)(3))
    println()
    println(lift[[X] =>> String](new Show)(if (true) 3 else 4))
    println(lift[[X] =>> X](new Eval)(if (true) 3 else 4))
    println()
    println(lift[[X] =>> String](new Show)(if (if (true) true else false) 3 else 4))
    println(lift[[X] =>> X](new Eval)(if (if (true) true else false) 3 else 4))
    println()
    println(lift[[X] =>> String](new Show)(if (3 <= 7) 3 else 4))
    println(lift[[X] =>> X](new Eval)(if (3 <= 7) 3 else 4))
    println()
    println(lift[[X] =>> String](new Show)(if (3 <= 7) 3 + 4 else 5 * 2))
    println(lift[[X] =>> X](new Eval)(if (3 <= 7) 3 + 4 else 5 * 2))
    println()
    println(lift[[X] =>> String](new Show)(((x: Int) => x + x) (4)))
    println(lift[[X] =>> X](new Eval)(((x: Int) => x + x) (4)))
    println()
    println(lift[[X] =>> String](new Show)(((x: Boolean) => if (x) 3 else 4) (true)))
    println(lift[[X] =>> X](new Eval)(((x: Boolean) => if (x) 3 else 4) (true)))
    println()
    println(lift[[X] =>> String](new Show)(if (((x: Int) => x <= x)(4)) 3 else 4))
    println(lift[[X] =>> X](new Eval)(if (((x: Int) => x <= x)(4)) 3 else 4))
    println()
    println(lift[[X] =>> String](new Show)(if (((b: Boolean) => b)(true)) 3 else 4))
    println(lift[[X] =>> X](new Eval)(if (((b: Boolean) => b)(true)) 3 else 4))
    println()
    println(lift[[X] =>> String](new Show)(((f: Int => Int) => f(4))((x: Int) => x)))
    println(lift[[X] =>> X](new Eval)(((f: Int => Int) => f(4))((x: Int) => x)))
    println()
    println(lift[[X] =>> String](new Show)(((x: Int) => Symantics.fix((self: Int => Int) => ((n: Int) => if (n <= 0) 1 else x * self(n + (-1)) )))(3)(25)))
    println(lift[[X] =>> X](new Eval)(((x: Int) => Symantics.fix((self: Int => Int) => ((n: Int) => if (n <= 0) 1 else x * self(n + (-1)) )))(3)(5)))
  }

}


class Show extends Symantics {
  type Repr[X] = String
  def int(x: Int): Repr[Int] = x.toString
  def bool(x: Boolean): Repr[Boolean] = x.toString
  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B] = {
    val i = nextIndex()
    s"(arg$i => ${f(s"arg$i")})"
  }
  def app[A, B](f: Repr[A => B], arg: Repr[A]): Repr[B] = s"$f($arg)"
  def fix[A, B]: (Repr[A => B] => Repr[A => B]) => Repr[A => B] = f => f("FIX")
  def add(x: Repr[Int], y: Repr[Int]): Repr[Int] = s"$x + $y"
  def mult(x: Repr[Int], y: Repr[Int]): Repr[Int] = s"($x) * ($y)"
  def leq(x: Repr[Int], y: Repr[Int]): Repr[Boolean] = s"$x <= $y"
  def ifThenElse[A](cond: Repr[Boolean], thenp: => Repr[A], elsep: => Repr[A]): Repr[A] = s"if ($cond) $thenp else $elsep"

  private[this] var idx: Int = 0
  private def nextIndex(): Int = {
    idx += 1
    idx
  }
}

class Eval extends Symantics {
  type Repr[X] = X
  def int(x: Int): Repr[Int] = x
  def bool(x: Boolean): Repr[Boolean] = x
  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B] = f
  def app[A, B](f: Repr[A => B], arg: Repr[A]): Repr[B] = f(arg)
  def fix[A, B]: (Repr[A => B] => Repr[A => B]) => Repr[A => B] = f => {
    def self(n: A): B = f(self)(n)
    self
  }
  def add(x: Repr[Int], y: Repr[Int]): Repr[Int] = x + y
  def mult(x: Repr[Int], y: Repr[Int]): Repr[Int] = x * y
  def leq(x: Repr[Int], y: Repr[Int]): Repr[Boolean] = x <= y
  def ifThenElse[A](cond: Repr[Boolean], thenp: => Repr[A], elsep: => Repr[A]): Repr[A] = if (cond) thenp else thenp

}
