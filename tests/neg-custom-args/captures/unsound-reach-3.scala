//> using options -source 3.4
// (to make sure we use the sealed policy)
import language.experimental.captureChecking
trait File:
  def close(): Unit

def withFile[R](path: String)(op: File^ => R): R = ???

trait Foo[+X]:
  def use(x: File^): X
class Bar extends Foo[File^]:
  def use(x: File^): File^ = x

def bad(): Unit =
  val backdoor: Foo[File^] = new Bar
  val boom: Foo[File^{backdoor*}] = backdoor

  var escaped: File^{backdoor*} = null
  withFile("hello.txt"): f =>
    escaped = boom.use(f) // error
      // boom.use: (x: File^) -> File^{backdoor*}, it is a selection so reach capabilities are allowed
      // f: File^, so there is no reach capabilities

