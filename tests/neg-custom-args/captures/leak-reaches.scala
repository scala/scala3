import caps.fresh

class File:
  def write(): Unit = ???

def usingFile[T](f: File^ => T): T = ???

type Proc = () => Unit

class Ref[T](init: T) extends caps.Stateful:
  private var x: T = init
  def get: T = x
  update def set(y: T) = { x = y }

def runAll2(consume xs: List[Proc]): Unit =
  var cur: List[Proc] = xs
  while cur.nonEmpty do
    val next: () => Unit = cur.head   // error
    next()
    cur = cur.tail

def runAll3(xs: List[Proc]): Unit =
  val cur = Ref[List[Proc]](xs) // error
  while cur.get.nonEmpty do
    val next: () => Unit = cur.get.head // error // error
    next()
    cur.set(cur.get.tail: List[Proc])

class Id[-A,  +B >: A]():
  def apply(a: A): B = a

def attack3 =
  val id: (x: File^) -> File^{fresh} = x => x // error
  val id2: File^ -> File^{fresh} = x => x // error

class List[+A]:
  def head: A = ???
  def tail: List[A] = ???
  def map[B](f: A => B): List[B] = ???
  def nonEmpty: Boolean = ???

extension [A](x: A) def :: (xs: List[A]): List[A] = ???

object Nil extends List[Nothing]

def compose1[A, B, C](f: A => B, g: B => C): A ->{f, g} C =
  z => g(f(z))

def mapCompose[A](ps: List[(A => A, A => A)]): List[A ->{ps*} A] =
  ps.map((x, y) => compose1(x, y)) // error // error // error

def mapCompose2[A, C^](ps: List[(A ->{C} A, A ->{C} A)]): List[A ->{C} A] =
  ps.map((x, y) => compose1(x, y)) // error
