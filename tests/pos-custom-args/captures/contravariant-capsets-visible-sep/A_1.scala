package p
import language.experimental.captureChecking

class File

object O:
  // Declared parameter types are pickled
  def mk(io: File^) = (f: File^{io}) => ()
  def mkSome(io: File^): Some[File^{io} -> Unit] = Some((f: File^{io}) => ())
  // `^` is pickled as it is written
  val hs = List((f: File^) => ())
  def mkAny() = Some((f: File^) => ())
