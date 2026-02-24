import language.experimental.captureChecking

trait Iterator[+A] extends IterableOnce[A]:
  self: Iterator[A]^ =>
  def next(): A

trait IterableOnce[+A] extends Any:
  def iterator: Iterator[A]^{this}

final class Cell[A](head: () => IterableOnce[A]^):
  def headIterator: Iterator[A]^{this} = head().iterator

class File private ():
  @caps.unsafe.untrackedCaptures private var closed = false

  def close() = closed = true

  def read() =
    assert(!closed, "File closed")
    1

object File:
  def open[T](f: File^ => T): T =
    val file = File()
    try
      f(file)
    finally
      file.close()

object Seq:
  def apply[A](xs: A*): IterableOnce[A] = ???

@main def Main() =
  val cell: Cell[File] = File.open(f => Cell(() => Seq(f))) // error
  val file = cell.headIterator.next()
  file.read()
