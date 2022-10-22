import caps.*

abstract class Foo:
  def foo: () => Unit = () => ()
  def bar: String = ""

class Bar extends Foo:
  override def foo = () => println("bar")
  override def bar = "bar"
  override def toString = bar

class Baz extends Bar:
  override def foo = () => println("baz")
  override def bar = "baz"
  //override def toString = bar

abstract class Message:
  protected def msg: String
  override def toString = msg

abstract class SyntaxMsg extends Message

class CyclicInheritance extends SyntaxMsg:
  def msg = "cyclic"


