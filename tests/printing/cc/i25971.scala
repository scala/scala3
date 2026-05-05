import language.experimental.captureChecking
import caps.*

class File

object Test:
  val f = [C^ <: {any}] => (xs: List[File^{C}]) => (ys: File^{C}) => (zs: File^{C}) => { val _ = ys; () }
  val g = (ys: File^) => (zs: File^) => { val _ = ys; () }
