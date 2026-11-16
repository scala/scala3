package p
import language.experimental.captureChecking
import caps.*

class File
class Src[+A]

// A pure upper bound of a type parameter makes its inferred type argument pure,
// but not the parts of that type argument
object Test:
  def id[T <: Src[File^]](t: T): T = t
  def c(x: File^, s: Src[File^{x}]): Src[File^{x}] = id(s)

  type PT = [T <: File^] -> (t: T) -> Unit
  def d(arr: Array[PT]) = arr.foreach(f => ())
