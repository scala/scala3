// community-build/community-projects/stdLib213/src/library/scala/math/Numeric.scala
// community-build/community-projects/stdLib213/src/library/scala/math/Integral.scala
// community-build/community-projects/stdLib213/src/library/scala/math/Fractional.scala


trait InlineNumeric[T]: // extends Numeric[T] // TODO can we do this?
  transparent inline def plus(inline x: T, inline y: T): T
  transparent inline def times(inline x: T, inline y: T): T
  // TODO add missing methods
object InlineNumeric:
  extension [T](inline x: T)(using inline num: InlineNumeric[T])
    transparent inline def +(inline y: T): T = num.plus(x, y)
    transparent inline def *(inline y: T): T = num.times(x, y)
    // TODO add missing methods

trait InlineIntegral[T] extends InlineNumeric[T]:
  transparent inline def quot(inline x: T, inline y: T): T
  // TODO add missing methods
object InlineIntegral:
  // TODO: how are these imported/composed with Numeric/Integral. Should the extension methods be defined in trait InlineIntegral?
  extension [T](inline lhs: T)(using inline int: InlineIntegral[T])
    transparent inline def /(inline rhs: T) = int.quot(lhs, rhs)
    // TODO add missing methods

// TODO: InlineFractional

given IntIsInlineIntegral: InlineIntegral[Int] with
  transparent inline def plus(inline x: Int, inline y: Int): Int = x + y
  transparent inline def times(inline x: Int, inline y: Int): Int = x * y
  transparent inline def quot(inline x: Int, inline y: Int): Int = x / y
  // TODO add missing methods

given ShortIsInlineIntegral: InlineIntegral[Short] with
  transparent inline def plus(inline x: Short, inline y: Short): Short = (x + y).toShort
  transparent inline def times(inline x: Short, inline y: Short): Short = (x * y).toShort
  transparent inline def quot(inline x: Short, inline y: Short): Short = (x / y).toShort
  // TODO add missing methods

// TODO add missing primitive types

object tests:
  import InlineNumeric.*

  // A generic inline operation that inlines/specializes primitive operations
  inline def foo[T: InlineNumeric](a: T, b: T) =
    a + b * b

  def test(a: Int, b: Int) =
    foo(a, b) // should be a + b * b // can check with -Xprint:inlining
    foo(a.toShort, b.toShort) // should be a + b * b
