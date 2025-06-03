trait JsValue
trait JsObject extends JsValue

trait Writer[T]
trait BodySerializer[-B]

class Printer

given Writer[JsValue] = ???
given Writer[JsObject] = ???

// This is not an exact desugaring of the original code: currently the compiler
// actually changes the modifier of the parameter list from `using` to
// `implicit` when desugaring the context-bound `B: Writer` to `implicit writer:
// Writer[B]`, but we can't write it in user code as this is not valid syntax.
given [B](using
    writer: Writer[B],
    printer: Printer = new Printer
): BodySerializer[B] = ???

def f: Unit =
  summon[BodySerializer[JsObject]] // error: Ambiguous given instances
