import language.experimental.captureChecking
import caps.*

class Ref extends Mutable:
  private var _data = 0
  def get: Int = _data
  update def put(x: Int): Unit = _data = x

case class Pair[+A, +B](fst: A, snd: B)

// require f and g to be non-interfering
def par(f: () => Unit, g: () => Unit): Unit = ???

def bad(consume op: () ->{any.rd} Unit): () => Unit = op

def test2(consume x: Ref^): Unit =
  val f: () ->{x.rd} Unit = () => x.get
  val rx: () => Unit = bad(f)  // hides x.rd in the resulting `any`
  x.put(42)  // error
  x.get      // ok rd/rd
  par(rx, () => x.put(42))  // error
  par(rx, () => x.get)  // ok rd/rd

def test3(consume x: Ref^): Unit =
  val f: () ->{x.rd} Unit = () => x.get
  def foo = bad(f) // error
  foo()
  foo()

def test4(consume p: Pair[Ref^, Ref^]): Ref^ = p.fst // error

/*
def test4(consume @use p: Pair[Ref^, Ref^]): Unit =
  val x: Ref^{p.fst*} = p.fst
  val y: Ref^{p.snd*} = p.snd
  badp(Pair(x, y))
  println(p.fst.get)

def badp(consume p: Pair[Ref^, Ref^]): Unit = ()

def test5(consume @use p: Pair[Ref^, Ref^]): Unit =
  badp(p) // ok
  println(p.fst.get)
*/
