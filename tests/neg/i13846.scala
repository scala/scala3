import language.experimental.saferExceptions

def foo(): Int throws ArithmeticException = 1 / 0 // error

def test(): Unit =
  try
    foo() // error
  catch
    case _: ArithmeticException => println("Caught")
