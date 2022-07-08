import scala.reflect.TypeTest

class ForSyntax[E](using E: TypeTest[E | Any, E]):
  extension [A](aOrE: E | A)
    inline def flatMap[B](f: A => E | B): E | B =
      aOrE match
        case e: E => ???
        case _ => ???


class UnhappyCase[E](using E: TypeTest[E | Any, E]) extends ForSyntax[E]:
  extension [A](aOrE: E | A)
    inline def fold[B](inline fe: E => B, inline fa: A => B): B =
      aOrE match
        case e: E => ???
        case _ => ???

class A:
  private val a = 1
  inline def foo() = a
class B extends A:
  private val a = 2
  inline def bar() = a
