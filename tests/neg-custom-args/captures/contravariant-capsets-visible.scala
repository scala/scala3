package p
import language.experimental.captureChecking

class File

// Inferred capture sets in contravariant positions are not pickled, so other
// compilation units would see `Some[File -> Unit]`
object O:
  def sink[T]: T -> Unit = _ => ()
  def mk1(io: File^) = Some((f: File^{io}) => ()) // error
  def mk2(io: File^) = Some(sink[File^{io}]) // error
  def mk3(io: File^) =
    val g = (f: File^{io}) => () // error
    Some(g)
