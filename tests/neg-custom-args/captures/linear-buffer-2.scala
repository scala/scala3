import caps.{any, Mutable}
import language.experimental.captureChecking

class Buffer[T] extends Mutable:
  consume def append(x: T): Buffer[T]^ = this // ok

def app[T](consume buf: Buffer[T]^, elem: T): Buffer[T]^ =
  buf.append(elem)

def Test(consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = buf.append(1)
  val buf2 = buf1.append(2) // OK
  val buf3 = buf.append(3) // error

def Test2(consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = buf.append(1)
  val buf2 =
    if ??? then buf1.append(2) // OK
    else buf1.append(3) // OK
  val buf3 = buf1.append(4) // error

def Test3(consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = buf.append(1)
  val buf2 = (??? : Int) match
    case 1 => buf1.append(2) // OK
    case 2 => buf1.append(2)
    case _ => buf1.append(3)
  val buf3 = buf1.append(4) // error

def Test4(consume buf: Buffer[Int]^) =
  val buf1: Buffer[Int]^ = buf.append(1)
  val buf2 = (??? : Int) match
    case 1 => buf1.append(2) // OK
    case 2 => buf1.append(2)
    case 3 => buf1.append(3)
    case 4 => buf1.append(4)
    case 5 => buf1.append(5)
  val buf3 = buf1.append(4) // error

def Test5(consume buf: Buffer[Int]^) =
  while true do
    buf.append(1)  // error
