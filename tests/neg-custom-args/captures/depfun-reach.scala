import language.experimental.captureChecking
import caps.any

trait List[+T]:
  def foreach(op: T => Unit): Unit = ???

object List:
  def apply[T](elem: T): List[T] = ???

def test(io: Object^, async: Object^) =
  def compose(op: List[(() ->{any} Unit, () ->{any} Unit)]): List[() ->{op*} Unit] =
    List(() => op.foreach((f,g) => { f(); g() })) // error (???)

  def compose1(op: List[(() ->{async} Unit, () ->{io} Unit)]): List[() ->{op*} Unit] =
    compose(op)

  def foo[X](op: (xs: List[(X, () ->{io} Unit)]) => List[() ->{xs*} Unit])
               : (xs: List[(X, () ->{io} Unit)]) => List[() ->{} Unit] =
    op // error

  def boom(op: List[(() ->{async} Unit, () ->{io} Unit)]): List[() ->{} Unit] =
    foo(compose1)(op)

def test2(io: Object^) =
  val a: (xs: List[() ->{io} Unit]) => List[() ->{xs*} Unit] = ???
  val b: (xs: List[() ->{io} Unit]) => List[() ->{} Unit] = a // error
