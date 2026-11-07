package p
import language.experimental.captureChecking

class File
class Ref[T](val elem: T)
class Sink[-T]

// The declared parameter types of these closures are pickled as they are written,
// so they are in the externally visible types
object O:
  def run(k: File^ => Unit): Unit = ()
  val h = (k: File^ => Unit) => ()
  val c = (f: File^) => ()
  val h1: (k: File^ => Unit) -> Unit = (k: File^ => Unit) => ()
  val v1 = (r: Ref[File^]) => ()
  val v2 = (s: Sink[File^]) => ()
  val v3 = run
  val v4 = [C^] => (k: File^{C} => Unit) => ()

class D(val x: File^):
  val v5 = (k: (f: File^{x}) -> Unit) => ()
