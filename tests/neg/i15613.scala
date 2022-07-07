import annotation.*

trait T
object X extends T

@implicitNotFound("No U[${A}]!")
trait U[A]

def f(using T) = ???

def test = f(X) // error

def noTypeTest = f(42) // error

def plainWrong = f() // error no addendum

def g(using T)(i: Int) = ???

def noTypeTestIsAnnoying = g(42) // error

def testCount = f(42, 27) // error no addendum

def multi(using T, Int) = ???

def testMulti = multi(X, 42) // error

def testBadMulti = multi(X, 42, 27) // error no addendum

def u[A](using U[A]) = ???

def testWithMessage = u[String] // error no addendum

def testWithMessageWithAddendum = u[String](42) // error with addendum
