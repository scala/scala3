class File:
  def write(): Unit = ???

def usingFile[sealed T](f: File^ => T): T = ???

type Proc = () => Unit

class Ref[sealed T](init: T):
  private var x: T = init
  def get: T = x
  def set(y: T) = { x = y }

def runAll0(xs: List[Proc]): Unit =
  var cur: List[() ->{xs*} Unit] = xs  // OK, by revised VAR
  while cur.nonEmpty do
    val next: () ->{xs*} Unit = cur.head
    next()
    cur = cur.tail: List[() ->{xs*} Unit]


  usingFile: f =>
    cur = (() => f.write()) :: Nil // error since {f*} !<: {xs*}

def runAll1(xs: List[Proc]): Unit =
  val cur = Ref[List[() ->{xs*} Unit]](xs)  // OK, by revised VAR
  while cur.get.nonEmpty do
    val next: () ->{xs*} Unit = cur.get.head
    next()
    cur.set(cur.get.tail: List[() ->{xs*} Unit])

  usingFile: f =>
    cur.set:
      (() => f.write()) :: Nil // error since {f*} !<: {xs*}

def runAll2(xs: List[Proc]): Unit =
  var cur: List[Proc] = xs // error: Illegal type for var
  while cur.nonEmpty do
    val next: () => Unit = cur.head
    next()
    cur = cur.tail

def runAll3(xs: List[Proc]): Unit =
  val cur = Ref[List[Proc]](xs) // error: illegal type for type argument to Ref
  while cur.get.nonEmpty do
    val next: () => Unit = cur.get.head
    next()
    cur.set(cur.get.tail: List[Proc])

class Id[sealed -A, sealed +B >: A]():
  def apply(a: A): B = a

def test =
  val id: Id[Proc, Proc] = new Id[Proc, () -> Unit] // error
  usingFile: f =>
    id(() => f.write())  // escape, if it was not for the error above