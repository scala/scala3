import language.experimental.captureChecking

def foo(c: AnyRef^, d: AnyRef^, e: AnyRef^): Unit =
  trait I
  object O:
    val x = c
    def foo = println(d)

  val w: O.type = O  // O is not pure
  val _: AnyRef = w // error on this line
  val _: AnyRef^{c} = w // error on this line, since d is also captured
  val _: AnyRef^{c, d} = w // ok

