


import language.experimental.captureChecking;


trait File:
  def close(): Unit

def withFile[R](path: String)(op: File^ => R): R = ???

type F = File^

trait Foo[+X]:
  def use(x: F): X
class Bar extends Foo[File^]: // error
  def use(consume x: F): File^ = x // error consume override

def bad(): Unit =
  val backdoor: Foo[File^] = new Bar // error (follow-on, since the parent Foo[File^] of bar is illegal).
  val boom: Foo[File^{backdoor*}] = backdoor

  var escaped: File^{backdoor*} = null
  withFile("hello.txt"): f =>
    escaped = boom.use(f)  // was error
