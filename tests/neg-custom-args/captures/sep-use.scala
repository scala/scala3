import caps.any


def test1(consume io: Object^): Unit =

  val x: () => Unit = () => println(io)
  println(io) // error
  println(x)  // ok

def test2(consume io: Object^): Unit =

  def x: () => Unit = () => println(io)  // error
  println(io) // error
  println(x)  // ok

def test3(consume io: Object^): Unit =

  def xx: (y: Int) => Unit = _ => println(io)  // error
  println(io) // error
  println(xx(2))  // ok

def test4(consume io: Object^): Unit =

  def xxx(y: Int): Object^ = io  // error
  println(io) // error
  println(xxx(2))  // ok

