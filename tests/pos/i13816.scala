import language.experimental.saferExceptions

class Ex1 extends Exception("Ex1")
class Ex2 extends Exception("Ex2")

def foo1(i: Int): Unit throws Ex1 throws Ex2 =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo2(i: Int): Unit throws Ex1 | Ex2 =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo3(i: Int): Unit throws (Ex1 | Ex2) =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo4(i: Int)(using CanThrow[Ex1], CanThrow[Ex2]): Unit =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo5(i: Int)(using CanThrow[Ex1])(using CanThrow[Ex2]): Unit =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo6(i: Int)(using CanThrow[Ex1 | Ex2]): Unit =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo7(i: Int)(using CanThrow[Ex1]): Unit throws Ex2 =
   if i > 0 then throw new Ex1 else throw new Ex2

def foo8(i: Int)(using CanThrow[Ex2]): Unit throws Ex1 =
   if i > 0 then throw new Ex1 else throw new Ex2

def test(): Unit =
  try
    foo1(1)
    foo2(1)
    foo3(1)
    foo4(1)
    foo5(1)
    foo6(1)
    foo7(1)
    foo8(1)
  catch
    case _: Ex1 =>
    case _: Ex2 =>
