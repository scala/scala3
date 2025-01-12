import caps.cap
import language.future // sepchecks on

def test1(io: Object^): Unit =

  val x: () => Unit = () => println(io)
  println(io) // error
  println(x)  // ok

def test2(io: Object^): Unit =

  def x: () => Unit = () => println(io)
  println(io) // error
  println(x)  // ok

def test3(io: Object^): Unit =

  def xx: (y: Int) => Unit = _ => println(io)
  println(io) // error
  println(xx(2))  // ok

def test4(io: Object^): Unit =

  def xxx(y: Int): Object^ = io
  println(io) // error
  println(xxx(2))  // ok

