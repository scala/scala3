import language.experimental.captureChecking
trait Consumer[-T]:
  def apply(x: T): Unit

trait File:
  def close(): Unit

def withFile[R](path: String)(op: Consumer[File]): R = ???

trait Foo[+X]:
  def use(x: File^)(op: Consumer[X]): Unit
class Bar extends Foo[File^]:
  def use(x: File^)(op: Consumer[File^]): Unit = op.apply(x)

def bad(): Unit =
  val backdoor: Foo[File^] = new Bar
  val boom: Foo[File^{backdoor*}] = backdoor

  var escaped: File^{backdoor*} = null
  withFile("hello.txt"): f =>
    boom.use(f): // error
      new Consumer[File^{backdoor*}]: // error
        def apply(f1: File^{backdoor*}) =
          escaped = f1

