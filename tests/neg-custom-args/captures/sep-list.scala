import caps.Mutable
import caps.cap

abstract class LIST[+T]:
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]
  def map[U](f: T => U): LIST[U] =
    if isEmpty then NIL
    else CONS(f(head), tail.map(f))

class CONS[+T](x: T, xs: LIST[T]) extends LIST[T]:
  def isEmpty = false
  def head = x
  def tail = xs
object NIL extends LIST[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  mut def put(y: Int): Unit = x = y

def listFresh(n: Int): LIST[Ref^] =
  if n == 0 then NIL
  else
    val hd = Ref()
    val tl = listFresh(n - 1)
    CONS(hd, tl)

def par(x: Ref^, y: Ref^): Unit = ()

def test =
  val xs = listFresh(10)
  val h1 = xs.head
  val h2 = xs.head
  par(h1, h2) // error
