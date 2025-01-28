import caps.{cap, consume, Mutable}
import language.experimental.captureChecking

class BadBuffer[T] extends Mutable:
  mut def append(x: T): BadBuffer[T]^ = this // error
  def foo = // error
    def bar: BadBuffer[T]^ = this // error
    bar

class Buffer[T] extends Mutable:
  @consume mut def append(x: T): Buffer[T]^ = this // ok

def app[T](@consume buf: Buffer[T]^, elem: T): Buffer[T]^ =
  buf.append(elem)

def Test(@consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = app(buf, 1)
  val buf2 = app(buf1, 2) // OK
  val buf3 = app(buf, 3) // error

def Test2(@consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = app(buf, 1)
  val buf2 =
    if ??? then app(buf1, 2) // OK
    else app(buf1, 3) // OK
  val buf3 = app(buf1, 4) // error

def Test3(@consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = app(buf, 1)
  val buf2 = (??? : Int) match
    case 1 => app(buf1, 2) // OK
    case 2 => app(buf1, 2)
    case _ => app(buf1, 3)
  val buf3 = app(buf1, 4) // error

def Test4(@consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = app(buf, 1)
  val buf2 = (??? : Int) match
    case 1 => app(buf1, 2) // OK
    case 2 => app(buf1, 2)
    case 3 => app(buf1, 3)
    case 4 => app(buf1, 4)
    case 5 => app(buf1, 5)
  val buf3 = app(buf1, 4) // error

def Test5(@consume buf: Buffer[Int]^) =
  while true do
    app(buf, 1)  // error
