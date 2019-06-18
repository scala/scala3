
import annotation._

@implicitNotFound("No C of ${ A }")
class C[A]

trait X$Y
/* using the $$ separator for expanded names is unwise
trait X$$Y
trait X$$$Y
trait X$$$$Y
 */

trait Foo[A]

trait U[X, Y[_], Z[_, ZZ]] {
  class I[R] {
    def m[S](implicit i: Int @implicitNotFound("${X} ${Y} ${ Z } ${R} ${S} -- ${XX}.")) = ???
  }
}

class Test {
  def f[A: C] = ???
  f[X$Y] // error
  /* using the $$ separator for expanded names is unwise
    f[X$$Y]
    f[X$$$Y]
    f[X$$$$Y]
   */
  f[Foo[Int]] // error

  def g[Aaa](implicit theC: C[Aaa]) = ???
  g[Foo[Int]] // error

  def h[Aaa](implicit theC: C[Aaa]  @implicitNotFound("I see no C[${Aaa}]")) = ???
  h[Foo[Int]] // error

  val u = new U[String, List, ({type T[A, _] = List[C[_]]})#T] { }
  val i = new u.I[Int]
  i.m[Option[Long]] // error
}