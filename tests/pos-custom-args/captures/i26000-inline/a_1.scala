package i26000inline

import language.experimental.captureChecking

class File
class Box[A](val value: A)
object io extends File, caps.SharedCapability

// The bodies of inline methods are inlined in other compilation units together with
// their inferred type arguments, so the inferred types in them stay as they are.
object Lib:
  def id[T](x: T): T = x
  inline def mono(): Int =
    val b = Box((y: File^{io}) => 1)
    val c = id(b)
    0
  inline def poly(): Int =
    val b = Box([C^] => (y: File^{C}) => 1)
    val c = List(b)
    0
