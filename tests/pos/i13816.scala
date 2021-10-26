import language.experimental.saferExceptions

class Ex1 extends Exception("Ex1")
class Ex2 extends Exception("Ex2")

def foo1(i: Int): Unit throws Ex1 throws Ex2 =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo2(i: Int): Unit throws Ex1 | Ex2 =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo2a(i: Int): Unit throws (Ex1 | Ex2) =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo3(i: Int)(using CanThrow[Ex1], CanThrow[Ex2]) =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo4(i: Int)(using CanThrow[Ex1])(using CanThrow[Ex2]) =
   if i > 0 then throw new Ex1 else throw new Ex2

//def foo5(i: Int)(using CanThrow[Ex1 & Ex2]) =      // does not work: no capability aggregation is supported
//   if i > 0 then throw new Ex1 else throw new Ex2

def test(): Unit =
  try {
    foo1(1)
    foo2(1)
    foo2a(1)
    foo3(1)
    foo4(1)
    //foo5(1) // error
  } catch {
    case _: Ex1 =>
    case _: Ex2 =>
  }