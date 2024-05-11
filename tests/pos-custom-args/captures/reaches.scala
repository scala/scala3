class C
def f(xs: List[C^]) =
  val y = xs
  val z: List[C^{xs*}] = y

type Proc = () => Unit

class Ref[T](init: T):
  private var x: T = init
  def get: T = x
  def set(y: T) = { x = y }

class List[+A]:
  def head: A = ???
  def tail: List[A] = ???
  def map[B](f: A -> B): List[B] = ???
  def nonEmpty: Boolean = ???

extension [A](x: A) def :: (xs: List[A]): List[A] = ???

object Nil extends List[Nothing]

def runAll(xs: List[Proc]): Unit =
  var cur: List[() ->{xs*} Unit] = xs  // OK, by revised VAR
  while cur.nonEmpty do
    val next: () ->{xs*} Unit = cur.head
    next()
    cur = cur.tail: List[() ->{xs*} Unit]

def id1(x: Proc): () ->{x} Unit = x
def id2(xs: List[Proc]): List[() ->{xs*} Unit] = xs

def cons(x: Proc, xs: List[Proc]): List[() ->{x, xs*} Unit] =
  val y = x :: xs
  y

def addOneProc(xs: List[Proc]): List[Proc] =
  val x: Proc = () => println("hello")
  val result: List[() ->{x, xs*} Unit] = x :: xs
  result // OK, we can widen () ->{x, xs*} Unit to cap here.

def compose1[A, B, C](f: A => B, g: B => C): A ->{f, g} C =
  z => g(f(z))

def compose2[A, B, C](f: A => B, g: B => C): A => C =
  z => g(f(z))

def mapCompose[A](ps: List[(A => A, A => A)]): List[A ->{ps*} A] =
  ps.map((x, y) => compose1(x, y)) // Does not work if map takes an impure function, see reaches in neg

class IO extends caps.Capability

def test(io: IO) =
  val a: () ->{io} Unit = () => ()
  val as: List[() ->{io} Unit] = Nil
  val bs: List[() ->{io} Unit] = cons(a, as)
  val cs = cons(a, as)
  val ds: List[() ->{io} Unit] = cs

