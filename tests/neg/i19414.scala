trait JsValue
trait JsObject extends JsValue

trait Writer[T]
trait BodySerializer[-B]

class Printer

given Writer[JsValue] = ???
given Writer[JsObject] = ???

given [B: Writer] => (printer: Printer = new Printer) => BodySerializer[B] = ???

def f: Unit =
  summon[BodySerializer[JsObject]] // error: Ambiguous given instances
