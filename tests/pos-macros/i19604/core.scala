// core.scala
package prelude

sealed trait Assertion[-A]
object Assertion:
  def greaterThanOrEqualTo[A](value: A): Assertion[A] = ???

sealed trait AssertionError

abstract class NewtypeCustom[A] {
  type Type
  protected inline def validateInline(inline value: A): Unit

  inline def apply(inline a1: A): Type = {
    validateInline(a1)
    a1.asInstanceOf[Type]
  }
}
abstract class Newtype[A] extends NewtypeCustom[A] {
  def assertion: Assertion[A] = ???
  protected inline def validateInline(inline value: A): Unit = ${
    Macros.validateInlineImpl[A]('assertion, 'value)
  }
}

abstract class Subtype[A] extends Newtype[A] {
  type Type <: A
}


package object newtypes {
  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    override inline def assertion = Assertion.greaterThanOrEqualTo(0)
    val one: Natural =  Natural(1) // triggers macro
  }
}