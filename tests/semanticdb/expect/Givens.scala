package a
package b

object Givens

  given :[A](any: A)
    def sayHello = s"Hello, I am $any"

  val hello1 = 1.sayHello

  trait Monoid[A]
    def empty: A
    def (x: A) combine (y: A): A

  given Monoid[String]
    def empty = ""
    def (x: String) combine (y: String) = x + y

  inline given int2String: Conversion[Int, String] = _.toString

  def foo[A](given A: Monoid[A]): A = A.combine(A.empty)(A.empty)
