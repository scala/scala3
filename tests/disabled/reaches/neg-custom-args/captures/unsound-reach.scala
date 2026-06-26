import language.experimental.captureChecking;
trait File:
  def close(): Unit

def withFile[R](path: String)(op: File^ => R): R = ???

trait Foo[+X]:
    def use(x: File^)(op: X => Unit): Unit
class Bar extends Foo[File^]: // error
    def use(x: File^)(op: File^ => Unit): Unit = op(x) // OK using sealed checking

abstract class Foo2[+X]():
    def use(x: File^)(op: X => Unit): Unit
class Bar2 extends Foo2[File^]: // no error, since we check only parent types, and Foo2[File^] is a constructor application
    def use(x: File^)(op: File^ => Unit): Unit = op(x) // OK using sealed checking

def bad(): Unit =
    val backdoor: Foo[File^] = new Bar   // error
    val boom: Foo[File^{backdoor*}] = backdoor

    var escaped: File^{backdoor*} = null
    withFile("hello.txt"): f =>
        boom.use(f): (f1: File^{backdoor*}) => // was error
            escaped = f1

def bad2(): Unit =
    val backdoor: Foo2[File^] = new Bar2   // error
    val boom: Foo2[File^{backdoor*}] = backdoor

    var escaped: File^{backdoor*} = null
    withFile("hello.txt"): f =>
        boom.use(f): (f1: File^{backdoor*}) => // was error
            escaped = f1

