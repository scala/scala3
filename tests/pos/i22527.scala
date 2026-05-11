//> using options -Werror

import annotation.*

def f: Unit =
  identity(
    identity:
      class X extends AnyRef, Serializable
      42
  )

def test: Unit =
  assert(
    identity:
      true,
    "ok"
  )

def toss: Unit =
  assert(
    throw
      null,
    "ok"
  )
def raise: Unit =
  assert(
    throw
      null, "ok" // ok now
  )

def callme[A](x: => A, msg: String) = try x.toString catch case t: RuntimeException => msg

def orElse(x: Int): Unit =
  callme(
    if x > 0 then
      true
    else
      false, "fail")

@nowarn("id=E190&msg=Discarded non-Unit value")
def onlyIf(x: Int): Unit =
  callme(
    if (x > 0) then // then syntax
      true, "fail") // warn value discard

def h(xs: List[Int]) =
  xs.foldLeft(0)
    (
      (acc, x) =>
          acc
        + x,
    )

def sum(x: Int, y: Int, z: Int) = x+y+z

def k(xs: List[Int], y: Int, z: Int) =
  xs.foldLeft(0)
    (
      (acc, x) =>
          sum(
            x
          + y
            + z,
            y,
            z,
          )
    )

object `arrow eol`:
  def f(g: Int => Int, i: Int): Int = g(i)
  def g(map: Int => Int): Int => Int = map
  def test =
    f(
      g: x =>
        x + 1, 42
    )

def test2: Unit =
  assert(
    identity:
      true, "ok" // NO error end of statement expected but ',' found
  )

def orElse2(x: Int): Unit =
  callme(
    if x > 0 then
      class X extends AnyRef, Serializable // NO error Not found: Serializable - did you mean Specializable?
      true // NO error ',' or ')' expected, but 'true' found
    else
      false, "fail")

def g: Unit =
  identity(
    x =
      class X extends AnyRef, Serializable // NO error
      27 // NO error
  )

@nowarn("id=E190&msg=Discarded non-Unit value")
def onlyIf2(x: Int): Unit =
  callme(
    if (x > 0)
      true, "fail") // NO error syntax is OK after old-style conditional
