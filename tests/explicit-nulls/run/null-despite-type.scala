// Under explicit nulls a value can be `null` at run time even though its type says
// it cannot: `compiletime.uninitialized`, a field read before its initializer has run,
// Java interop, and `unsafeNulls` code all produce such values. Code generation must
// therefore not rely on the declared types to skip a null check.

import scala.compiletime.uninitialized

case class Box[A](x: A)

// The pattern match runs while `b` is still null, because the initializer of `r`
// in the superclass runs before the `b` in the subclass is assigned.
abstract class Sup:
  def b: Box[Int]
  val r: String = b match
    case Box(y) => s"box $y"
    case _      => "other"

class Sub extends Sup:
  val b = Box(1)

// No `toString` override, so the call goes through `Any_toString`, which
// `InterceptedMethods` rewrites.
class C(val n: Int)

abstract class Guarded:
  def b: C
  val r: String = b match
    case c: C if c.n > 0 => "pos"
    case _               => "other"

class GuardedSub extends Guarded:
  val b = new C(1)

object Test:
  var c: C = uninitialized

  def attempt(op: => String): String =
    try op catch case e: NullPointerException => "threw NPE"

  def main(args: Array[String]): Unit =
    // PatternMatcher must keep the null test on the scrutinee
    assert(attempt((new Sub).r) == "other", attempt((new Sub).r))

    // InterceptedMethods must route to the null-safe helpers
    assert(attempt(s"[${c.toString}]") == "[null]", attempt(s"[${c.toString}]"))
    // `ScalaRunTime.anyClass` answers `classOf[Null]` for a null receiver
    assert(attempt(s"${c.getClass}") == "class scala.runtime.Null$", attempt(s"${c.getClass}"))

    // TypeTestsCasts must not fold a type test to `true` just because the
    // declared type already matches.
    assert(attempt((new GuardedSub).r) == "other", attempt((new GuardedSub).r))
