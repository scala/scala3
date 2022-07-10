package pkg

import annotation.*

class myAnnot extends StaticAnnotation

@implicitNotFound("Missing Outer[${ A }] with OuterMember = ${OuterMember}")
class Outer[A] {
  type OuterMember

  @implicitNotFound("Missing Inner[${B}, ${C}] with InnerMember = ${InnerMember} from Outer[${ A }] with OuterMember = ${OuterMember}")
  class Inner[B, C[_]] {
    type InnerMember
  }
}

trait X$Y

@implicitNotFound(msg = "There's no U[${X}, ${Y}, ${Z}]")
trait U[X, Y[_], Z[_, ZZ]] {
  class I[R] {
    def m[S](implicit @implicitNotFound("There is no '${XX}' but it's unchecked; ${X}; ${Y}; ${ Z }; ${R}; ${S}") i: Int) = ???
  }
}

// No refcheck of XX in presence of typer errors; trailing space in previous message for I.m was especially weird.
@implicitNotFound(msg = "There's no U[${X}, ${Y}, ${Z}] or ${XX}")
trait Unchecked[X, Y[_], Z[_, ZZ]]

class Test[A] {
  def f(implicit @implicitNotFound(msg = "Missing X$Y for Test[${A}]") xy: X$Y) = ???
  def g[B: Outer] = ???
  def h[B](implicit outer: Outer[B]) = ???
  def i[B](implicit @implicitNotFound("Missing implicit outer param of type Outer[${B}] for Test[${A}]") outer: Outer[B]) = ???
  def j[B, C, D[_]](implicit inner: Outer[B]#Inner[C, D]) = ???
  def k[B, C, D[_]](implicit @implicitNotFound("Missing implicit inner param of type Outer[${B}]#Inner[${C}, ${D}] for Test[${A}]") inner: Outer[B]#Inner[C, D]) = ???
}

object Test {
  val test = new Test[Char]

  test.f // error
  test.g[Int] // error
  test.h[X$Y] // error
  test.i[Option[String]] // error
  test.j[(Long, Long), Int | String, Array] // error
  test.k[Either[String, Any], Seq[Seq[Char]], Vector] // error

  implicitly[Outer[Option[String] | List[Iterable[Char]]] { type MyType = BigDecimal }] // error
  implicitly[(Outer[Option[String] | List[Iterable[Char]]] { type MyType = BigDecimal })#Inner[Byte, Seq]] // error
  implicitly[Outer[Int] @myAnnot] // error

  val outer = new Outer[Int] { type OuterMember = Long }
  val inner = new outer.Inner[Long, Iterator] { type InnerMember = Byte }

  implicitly[Outer[Int] { type OuterMember = Long }] // error
  implicitly[outer.type] // error
  implicitly[(Outer[Int] { type OuterMember = Long })#Inner[Long, Iterator] { type InnerMember = Byte }] // error
  implicitly[outer.Inner[Long, Iterator] { type InnerMember = Byte }] // error
  implicitly[inner.type] // error

  implicitly[U[Int, Option, Map]] // error

  val u = new U[String, List, [A, _] =>> List[Option[_]]] { }
  val i = new u.I[Int]
  i.m[Option[Long]] // error
}
