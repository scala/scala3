//> using options -experimental

import scala.annotation.unroll
import scala.deriving.Mirror

class Unrolled {
  final def foo(
    s: String,
    @unroll y: Boolean = true,
    @unroll i: Int = 0,
    @unroll c: Char = '?'): String = s + y + i + c
}

class Outer {
  class Unrolled {
    final def bar(
      s: String,
      @unroll y: Boolean = true,
      @unroll i: Int = 0,
      @unroll c: Char = '?'): String = s + y + i + c
  }

  case class UnrolledCase(
    s: String,
    @unroll y: Boolean = true,
    @unroll i: Int = 0,
    @unroll c: Char = '?') {
    def baz: String = s + y + i + c
  }

  class UnrolledSecondary {
    private var msg = ""

    def qux: String = msg

    def this(
      s: String,
      @unroll y: Boolean = true,
      @unroll i: Int = 0,
      @unroll c: Char = '?') = {
      this()
      msg = s + y + i + c
    }
  }
}

@main def Test: Unit =
  assert(Unrolled().foo("foo") == "footrue0?")
  assert(Unrolled().foo("foo", false) == "foofalse0?")
  assert(Unrolled().foo("foo", false, 1) == "foofalse1?")
  assert(Unrolled().foo("foo", false, 1, '@') == "foofalse1@")
  val outer = new Outer()
  assert(new outer.Unrolled().bar("bar") == "bartrue0?")
  assert(new outer.Unrolled().bar("bar", false) == "barfalse0?")
  assert(new outer.Unrolled().bar("bar", false, 1) == "barfalse1?")
  assert(new outer.Unrolled().bar("bar", false, 1, '@') == "barfalse1@")
  assert(outer.UnrolledCase.apply("baz").baz == "baztrue0?")
  assert(outer.UnrolledCase.apply("baz", false).baz == "bazfalse0?")
  assert(outer.UnrolledCase.apply("baz", false, 1).baz == "bazfalse1?")
  assert(outer.UnrolledCase.apply("baz", false, 1, '@').baz == "bazfalse1@")
  assert((new outer.UnrolledCase("baz")).baz == "baztrue0?")
  assert((new outer.UnrolledCase("baz", false)).baz == "bazfalse0?")
  assert((new outer.UnrolledCase("baz", false, 1)).baz == "bazfalse1?")
  assert((new outer.UnrolledCase("baz", false, 1, '@')).baz == "bazfalse1@")
  assert(summon[Mirror.Of[outer.UnrolledCase]].fromProduct(Tuple("baz")).baz == "baztrue0?")
  assert(summon[Mirror.Of[outer.UnrolledCase]].fromProduct(("baz", false)).baz == "bazfalse0?")
  assert(summon[Mirror.Of[outer.UnrolledCase]].fromProduct(("baz", false, 1)).baz == "bazfalse1?")
  assert(summon[Mirror.Of[outer.UnrolledCase]].fromProduct(("baz", false, 1, '@')).baz == "bazfalse1@")
  assert(new outer.UnrolledSecondary("qux").qux == "quxtrue0?")
  assert(new outer.UnrolledSecondary("qux", false).qux == "quxfalse0?")
  assert(new outer.UnrolledSecondary("qux", false, 1).qux == "quxfalse1?")
  assert(new outer.UnrolledSecondary("qux", false, 1, '@').qux == "quxfalse1@")
