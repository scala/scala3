//> using options -feature -preview

import Conversion.into

into trait T
class C(x: Int) extends T

object K:
  into opaque type A = C
  given Conversion[Int, A] = C(_)

object Test:
  given Conversion[Int, C] = C(_)

  def f(x: T) = ()
  def g(x: C) = ()
  f(1)      // ok
  g(1)      // warn

  import K.*
  def h(x: A) = ()
  h(1)

  into class Keyword(str: String)
  given stringToKeyword: Conversion[String, Keyword] = Keyword(_)

  val dclKeywords = Set[Keyword]("def", "val") // ok
  val keywords = dclKeywords + "if" + "then" + "else" // ok
