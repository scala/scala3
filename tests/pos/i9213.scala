trait A(a: Any, b: Int)
trait B(a: Any, b: Int):
  var x = 0
class C(a: String, b: Int)

object O extends
  C(b = 0, a = String("")),
  A(b = 0, a = String("")),
  B(b = 0, a = String(""))
