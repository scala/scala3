package p
import language.experimental.captureChecking
import caps.*

class File
class Box[A](val a: A)

// Upper bounds of type parameters in inferred types can be `any`,
// and are solved to the bounds they are compared with
object Test:
  type PC = [C^] -> (x: File^{C}) -> File^{x}
  type PT = [T <: File^] -> (t: T) -> Unit
  def usePC(f: PC) = ()
  def usePT(f: PT) = ()

  def some(b: Box[PC]) =
    val o = Some(b)
    usePC(o.get.a)

  def ident(b: Box[PT]) =
    val b1 = identity(b)
    usePT(b1.a)

  // the inferred type of an inline method is checked against the type seen in other
  // compilation units, which keeps the bound `{any}` of `C`
  inline def mk() = Box([C^] => (f: File^{C}) => f)
