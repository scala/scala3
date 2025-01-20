import language.experimental.saferExceptions
import language.experimental.modularity

class FileSystem extends caps.Capability

class Logger(using tracked val fs: FileSystem):
  def log(s: String): Unit = ???

def test(using fs: FileSystem) =
  val l: Logger^{fs} = Logger(using fs)
  l.log("hello world!")
  val xs: LazyList[Int]^{l} =
    LazyList.from(1)
      .map { i =>
        l.log(s"computing elem # $i")
        i * i
      }

trait LazyList[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: LazyList[A]^{this}

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???

final class LazyCons[+T](val x: T, val xs: () => LazyList[T]^) extends LazyList[T]:
  def isEmpty = false
  def head = x
  def tail: LazyList[T]^{this} = xs()
end LazyCons

extension [A](x: A)
  def #::(xs1: => LazyList[A]^): LazyList[A]^{xs1} =
    LazyCons(x, () => xs1)

extension [A](xs: LazyList[A]^)
  def map[B](f: A => B): LazyList[B]^{xs, f} =
    if xs.isEmpty then LazyNil
    else f(xs.head) #:: xs.tail.map(f)

object LazyList:
  def from(start: Int): LazyList[Int] =
    start #:: from(start + 1)

class Pair[+A, +B](x: A, y: B):
  def fst: A = x
  def snd: B = y

def test2(ct: CanThrow[Exception], fs: FileSystem) =
  def x: Int ->{ct} String = ???
  def y: Logger^{fs} = ???
  def p = Pair[Int ->{ct} String, Logger^{fs}](x, y)
  def p3 = Pair(x, y)
  def f = () => p.fst


/*
  val l1: Int => String = ???
  val l2: Object^{c} = ???
  val pd = () => Pair(l1, l2)
  val p2: Pair[Int => String, Object]^{c} = pd()
  val hd = () => p2.fst

*/