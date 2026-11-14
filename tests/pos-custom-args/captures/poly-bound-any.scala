package p
import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

// `any` in the bound of `C` stays as it is, it is not mapped to a local root
// of `mk`, which would become `fresh` in its result type
object Test:
  def test(x: File^) =
    def mk(): Box[[C^] -> (f: File^{C}) -> File^{f}] = Box([C^] => (f: File^{C}) => f)
    val r: File^{x} = mk().value[{x}](x)
