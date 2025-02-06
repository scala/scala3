
def f: Unit =
  identity(
    identity:
      class X extends AnyRef, Serializable
      42
  )

def g: Unit =
  identity(
    x =
      class X extends AnyRef, Serializable
      27
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

def callme[A](x: => A, msg: String) = try x.toString catch case t: RuntimeException => msg

def orElse(x: Int): Unit =
  callme(
    if x > 0 then
      true
    else
      false, "fail")

def onlyIf(x: Int): Unit =
  callme(
    if (x > 0)
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
