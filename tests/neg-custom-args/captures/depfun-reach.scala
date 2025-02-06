import language.experimental.captureChecking
import caps.cap

def test(io: Object^, async: Object^) =
  def compose(op: List[(() ->{cap} Unit, () ->{cap} Unit)]): List[() ->{op*} Unit] =
    List(() => op.foreach((f,g) => { f(); g() }))

  def compose1(op: List[(() ->{async} Unit, () ->{io} Unit)]): List[() ->{op*} Unit] =
    compose(op)

  def foo[X](op: (xs: List[(X, () ->{io} Unit)]) => List[() ->{xs*} Unit])
               : (xs: List[(X, () ->{io} Unit)]) => List[() ->{} Unit] =
    op // error

  def boom(op: List[(() ->{async} Unit, () ->{io} Unit)]): List[() ->{} Unit] =
    foo(compose1)(op)

def test2(io: Object^) =
  val a: (xs: List[() ->{io} Unit]) => List[() ->{xs*} Unit] = ???
  val b: (xs: List[() ->{io} Unit]) => List[() ->{} Unit] = a // error
