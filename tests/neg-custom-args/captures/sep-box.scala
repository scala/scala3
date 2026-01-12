import caps.Mutable
import caps.any

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
  update def put(y: Int): Unit = x = y

class Box[+X](val value: X)

def listFresh(n: Int): LIST[Box[Ref^]] =
  if n == 0 then NIL
  else
    val hd = Ref()
    val tl = listFresh(n - 1)
    CONS(Box(hd), tl)

def par(x: Ref^, y: Ref^): Unit = ()

def test =
  val xs = listFresh(10)
  val h1 = xs.head
  val h2 = xs.head
  par(h1.value, h2.value) // error
