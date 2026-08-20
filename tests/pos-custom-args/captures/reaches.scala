class C
def f[c^](xs: List[C^{c}]) =
  val y = xs
  val z: List[C^{c}] = y

type Proc = () => Unit

class Ref[T](init: T) extends caps.Mutable:
  private var x: T = init
  def get: T = x
  update def set(y: T) = { x = y }

class List[+A]:
  def head: A = ???
  def tail: List[A] = ???
  def map[B](f: A -> B): List[B] = ???
  def nonEmpty: Boolean = ???

extension [A](x: A) def :: (xs: List[A]): List[A] = ???

object Nil extends List[Nothing]

def runAll[C^](xs: List[() ->{C} Unit]): Unit =
  var cur: List[() ->{C} Unit] = xs  // OK, by revised VAR
  while cur.nonEmpty do
    val next: () ->{C} Unit = cur.head
    next()
    cur = cur.tail: List[() ->{C} Unit]

def id1(x: Proc): () ->{x} Unit = x
def id2[c^](xs: List[() ->{c} Unit]): List[() ->{c} Unit] = xs

def cons[c^](x: () ->{c} Unit, xs: List[() ->{c} Unit]): List[() ->{c} Unit] =
  val y = x :: xs
  y

def addOneProc(consume xs: List[Proc]): List[Proc] =
  val x: Proc = () => println("hello")
  val result = x :: xs
  result // OK, we can widen () ->{x, xs*} Unit to any here.

def compose1[A, B, C](f: A => B, g: B => C): A ->{f, g} C =
  z => g(f(z))

def compose2[A, B, C](consume f: A => B, consume g: B => C): A => C =
  z => g(f(z))

class IO extends caps.SharedCapability

def test(io: IO) =
  val a: () ->{io} Unit = () => ()
  val as: List[() ->{io} Unit] = Nil
  val bs: List[() ->{io} Unit] = cons(a, as)
  val cs = cons(a, as)
  val ds: List[() ->{io} Unit] = cs

