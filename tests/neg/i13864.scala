import language.experimental.saferExceptions

case class Ex[A](a: A) extends Exception(s"Ex: $a")

def foo[A](a: A): Unit throws Ex[A] = throw new Ex(a)

def test(): Unit =
  try
    foo(1) // error
  catch
    case Ex(i: Int) => println("Caught an Int") // error
