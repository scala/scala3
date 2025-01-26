
import language.experimental.captureChecking
import caps.consume

trait File:
  def close(): Unit

def withFile[R](path: String)(op: File^ => R): R = ???

trait Foo[+X]:
  def use(@consume x: File^): X
class Bar extends Foo[File^]: // error
  def use(@consume x: File^): File^ = x

def bad(): Unit =
  val backdoor: Foo[File^] = new Bar // error (follow-on, since the parent Foo[File^] of bar is illegal).
  val boom: Foo[File^{backdoor*}] = backdoor

  var escaped: File^{backdoor*} = null
  withFile("hello.txt"): f =>
    escaped = boom.use(f) // error
      // boom.use: (x: File^) -> File^{backdoor*}, it is a selection so reach capabilities are allowed
      // f: File^, so there is no reach capabilities

