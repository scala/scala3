import language.experimental.captureChecking; import language.`3.7` // sepchecks on
trait File:
  def close(): Unit

def withFile[R](path: String)(op: File^ => R): R = ???

trait Foo[+X]:
    def use(x: File^)(op: X => Unit): Unit
class Bar extends Foo[File^]: // error
    def use(x: File^)(op: File^ => Unit): Unit = op(x) // OK using sealed checking

abstract class Foo2[+X]():
    def use(x: File^)(op: X => Unit): Unit
class Bar2 extends Foo2[File^]: // error
    def use(x: File^)(op: File^ => Unit): Unit = op(x) // OK using sealed checking

def bad(): Unit =
    val backdoor: Foo[File^] = new Bar   // error (follow-on, since the parent Foo[File^] of bar is illegal).
    val boom: Foo[File^{backdoor*}] = backdoor

    var escaped: File^{backdoor*} = null
    withFile("hello.txt"): f =>
        boom.use(f): (f1: File^{backdoor*}) => // error
            escaped = f1

