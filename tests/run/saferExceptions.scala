import language.experimental.saferExceptions

class Fail extends Exception

def foo(x: Int) =
  try x match
    case 1 => throw AssertionError()
    case 2 => throw Fail()
    case 3 => throw java.io.IOException()
    case 4 => throw Exception()
    case 5 => throw Throwable()
    case _ => 0
  catch
    case ex: AssertionError => 1
    case ex: Fail => 2
    case ex: java.io.IOException => 3
    case ex: Exception => 4
    case ex: Throwable => 5

def bar(x: Int): Int throws Exception =
  x match
    case 1 => throw AssertionError()
    case 2 => throw Fail()
    case 3 => throw java.io.IOException()
    case 4 => throw Exception()
    case _ => 0

@main def Test =
  assert(foo(1) + foo(2) + foo(3) + foo(4) + foo(5) + foo(6) == 15)
  import unsafeExceptions.canThrowAny
  val x =
    try bar(2)
    catch case ex: Fail => 3 // OK
  assert(x == 3)
