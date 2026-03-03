package test
import language.experimental.safe
import caps.unsafe.untrackedCaptures
import scala.annotation.unchecked.{uncheckedCaptures, uncheckedVariance}

object B:
  def foo() = A.foo()

  val a = A()
  val o = new Object()

  val c = identity[String]
  println("hello") // error

  val u = Unsafe() // error
  Unsafe.foo() // error

  scala.Console.out.println("!") // error

  def bar(x: Unsafe): Unit =
    x.foo()
    x.a.foo()   // error
    import x.a.bam
    bam()  // error
