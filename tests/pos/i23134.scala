import scala.compiletime.summonInline

// Test for issue #23134.
object i23134:
  trait A:
    inline def foo: Int

  given a: A with
    override inline def foo: Int = 1

  summon[A].foo

  inline def summonAAndFoo: Int = summonInline[A].foo

  summonAAndFoo

object i23134_overloaded:
  trait A:
    inline def foo(x: Int): Int

  given a: A with
    override inline def foo(x: Int): Int = x + 1
    inline def foo(x: String): String = x

  inline def summonAAndFoo: Int = summonInline[A].foo(1)

  summonAAndFoo

object i23134_refined:
  trait A:
    type Out <: Int
    inline def foo: Out

  type Aux = A { type Out = 1 }

  given a: A with
    type Out = 1
    override inline def foo: 1 = 1

  inline def summonAAndFoo: 1 = summonInline[Aux].foo

  summonAAndFoo

object i23134_precise_return:
  trait A:
    inline def foo: Any

  given a: A with
    override inline def foo: String = "ok"

  val x1: String = summon[A].foo
  val x2: String = summonInline[A].foo

object i23134_literal_out:
  trait TC:
    type Out
    inline def out: Out

  given tc: TC with
    type Out = "ok"
    override inline def out: "ok" = "ok"

  val x1: "ok" = summon[TC].out
  val x2: "ok" = summonInline[TC].out

object i23134_int_out:
  trait A:
    type Out
    inline def value: Out

  given a: A with
    type Out = Int
    override inline def value: Int = 1

  val x1: Int = summon[A].value
  val x2: Int = summonInline[A].value
