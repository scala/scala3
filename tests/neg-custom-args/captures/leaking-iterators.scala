package cctest
import java.io.*

trait IterableOnce[+A]:
  //this: IterableOnce[A]^ =>
  def iterator: Iterator[A]^{this}

trait Iterable[+A] extends IterableOnce[A]:
  //this: IterableOnce[A]^ =>
  def iterator: Iterator[A]^{this}

trait List[+A] extends Iterable[A]:
  def head: A
  def tail: List[A]
  def length: Int
  def foldLeft[B](z: B)(op: (B, A) => B): B
  def foldRight[B](z: B)(op: (A, B) => B): B
  def foreach(f: A => Unit): Unit
  def iterator: Iterator[A]
  def map[B](f: A => B): List[B]
  def flatMap[B](f: A => IterableOnce[B]^): List[B]
  def ++[B >: A](xs: IterableOnce[B]^): List[B]
object List:
  def apply[A](xs: A*): List[A] = ???

trait Iterator[+A] extends IterableOnce[A]:
  this: Iterator[A]^ =>
  def hasNext: Boolean
  def next(): A
  def foldLeft[B](z: B)(op: (B, A) => B): B
  def foldRight[B](z: B)(op: (A, B) => B): B
  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): Iterator[B]^{this, f}
  def flatMap[B](f: A => IterableOnce[B]^): Iterator[B]^{this, f}
  def ++[B >: A](xs: IterableOnce[B]^): Iterator[B]^{this, xs}
end Iterator

private final class ConcatIteratorCell[A](head: => IterableOnce[A]^):
  def headIterator: Iterator[A]^{this} = head.iterator

def usingLogFile[sealed R](op: FileOutputStream^ => R): R =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

def test =
  val xs = List(1, 2, 3)

  usingLogFile: log =>
    xs.map: x =>
      log.write(x)
      x * x

  usingLogFile: log => // error
    xs.iterator.map: x =>
      log.write(x)
      x * x

