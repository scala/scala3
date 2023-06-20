object test1:
  trait A
  type B <: AnyRef
  val test1: A ?=> B = null // error
  val test2: A ?=> B = A ?=> null // error

import scala.quoted.*

object Eg1 {

  // no default arg: ok
  def ok  (f: (q: Quotes) ?=> q.reflect.Term) = ()

  // default the function *reference* to null: compilation error
  def ko_1(f: (q: Quotes) ?=> q.reflect.Term = null) = ()  // error

  // default the function *result* to null: compilation error
  def ko_2(f: (q: Quotes) ?=> q.reflect.Term = (_: Quotes) ?=> null) = () // error
}

