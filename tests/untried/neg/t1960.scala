object ClassFormatErrorExample extends App { new Aclass(1) }

trait TBase { var p:Int = 0; def f(p1: Int): Unit = {} }

class Aclass (p: Int) extends TBase { def g(): Unit = { f(p) } }
