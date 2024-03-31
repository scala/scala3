import language.experimental.captureChecking
trait File:
  def close(): Unit

def withFile[R](path: String)(op: File^ => R): R = ???

trait Foo[+X]:
  def use(x: File^)(op: X => Unit): Unit
class Bar extends Foo[File^]:
  def use(x: File^)(op: File^ => Unit): Unit = op(x)

def bad(): Unit =
  val backdoor: Foo[File^] = new Bar
  val boom: Foo[File^{backdoor*}] = backdoor

  var escaped: File^{backdoor*} = null
  withFile("hello.txt"): f =>
    boom.use(f): (f1: File^{backdoor*}) => // error
      escaped = f1

