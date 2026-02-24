import caps.fresh

class File:
  def write(): Unit = ???

def usingFile[T](f: File^ => T): T = ???

type Proc = () => Unit

class Ref[T](init: T) extends caps.Stateful:
  private var x: T = init
  def get: T = x
  update def set(y: T) = { x = y }

def runAll0[C^](xs: List[() ->{C} Unit]): Unit =
  var cur: List[() ->{C} Unit] = xs
  while cur.nonEmpty do
    val next: () ->{C} Unit = cur.head
    next()
    cur = cur.tail: List[() ->{C} Unit]

  usingFile: f =>
    cur = (() => f.write()) :: Nil  // error

def runAll1[C^](xs: List[() ->{C} Unit]): Unit =
  val cur = Ref[List[() ->{C} Unit]](xs)  // OK, by revised VAR
  while cur.get.nonEmpty do
    val next: () ->{C} Unit = cur.get.head
    next()
    cur.set(cur.get.tail: List[() ->{C} Unit])

  usingFile: f =>
    cur.set:
      (() => f.write()) :: Nil // error

def runAll2(consume xs: List[Proc]): Unit =
  var cur: List[Proc] = xs
  while cur.nonEmpty do
    val next: () => Unit = cur.head   // error, use
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

def test =
  val id: Id[Proc, Proc] = new Id[Proc, () -> Unit] // error
  usingFile: f =>
    id(() => f.write()) // was error

def attack2 =
  val id: File^ -> File^ = x => x // error
    // val id: File^ -> File^{fresh}

  val leaked = usingFile[File^{id*}]: f =>
    val f1: File^{id*} = id(f)
    f1

def attack3 =
  val id: (x: File^) -> File^{fresh} = x => x // was error, now OK
  val id2: File^ -> File^{fresh} = x => x // now also OK

  val leaked = usingFile[File^{id*}]: f =>
    val f1: File^{id*} = id(f)   // error
    f1

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
