package p
import language.experimental.captureChecking

class File
class Ref[T](val elem: T)
class Sink[-T]

object O:
  def run(k: File^ => Unit): Unit = ()
  val h = (k: File^ => Unit) => ()
  val v1 = (r: Ref[File^]) => ()
  val v2 = (s: Sink[File^]) => ()
  val v3 = run
  val v4 = [C^] => (k: File^{C} => Unit) => ()

class D(val x: File^):
  val v5 = (k: (f: File^{x}) -> Unit) => ()
