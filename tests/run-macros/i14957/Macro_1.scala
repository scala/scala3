import scala.quoted.*

inline def asSeenFromTest: String = ${ impl }

private def impl(using Quotes): Expr[String] =
  import quotes.reflect.*
  val aSym = Symbol.requiredClass("A")
  val xSym = aSym.methodMember("x").head
  // =>D[B] as seen from C with owner A
  Expr(xSym.info.asSeenFrom(TypeRepr.of[C], aSym).show)


trait D[E]

class A[B] {
  def x: D[B] = ???
}

class C extends A[Int]:
  override def x: D[Int] = ???
