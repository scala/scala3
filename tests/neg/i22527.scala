
//rule of thumb is COLONeol was at EOL, so COMMA must be at EOL
def test: Unit =
  assert(
    identity:
      true, "ok" // error end of statement expected but ',' found
  )

def callme[A](x: => A, msg: String) = try x.toString catch case t: RuntimeException => msg

// not all indented regions require COMMA at EOL for OUTDENT
def orElse(x: Int): Unit =
  callme(
    if x > 0 then
      class X extends AnyRef, Serializable // error Not found: Serializable - did you mean Specializable?
      true // error ',' or ')' expected, but 'true' found
    else
      false, "fail")

def g: Unit =
  identity(
    x =
      class X extends AnyRef, Serializable // error
      27 // error
  )
