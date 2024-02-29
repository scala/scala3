import language.experimental.saferExceptions

class Ex1 extends Exception("Ex1")
class Ex2 extends Exception("Ex2")

def foo(i: Int): (CanThrow[Ex1], CanThrow[Ex2]) ?-> Unit =
  if i > 0 then throw new Ex1 else throw new Ex2

class Cell[+T](val x: T)

def test(): Unit =
  try  // error
    () => foo(1)
  catch
    case _: Ex1 => ???
    case _: Ex2 => ???

  val x = try // error
    () => foo(1)
  catch
    case _: Ex1 => ???
    case _: Ex2 => ???

  val y = try  // error
    () => Cell(foo(1))
  catch
    case _: Ex1 => ???
    case _: Ex2 => ???

  val b = try // error
    Cell(() => foo(1))
  catch
    case _: Ex1 => ???
    case _: Ex2 => ???

  b.x
