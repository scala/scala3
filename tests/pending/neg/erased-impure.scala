//> using options -explain
import language.experimental.erasedDefinitions
import java.io.IOException
import caps.unsafe.unsafeErasedValue

class CanThrow[-E <: Exception]

def foo[E <: Exception](e: E)(using erased CanThrow[E]): Nothing = throw e

erased val magic1: IOException = ???  // error
erased val magic2: IOException = compiletime.erasedValue[IOException]  // error
erased val magic3: IOException = null.asInstanceOf[IOException]  // error

inline def inlineId[T](x: T) = x

class C()

def testPure[T](erased x: T): Unit = ()

case class Pair[A, B](x: A, y: B)
object Pair:
  def apply(x: Int): Pair2[Int, Int] =
    println("Pair2")
    Pair2(x, x + 1)

case class Box[A](x: A):
  println(x)

def Test =
  foo(new IOException)(using ???)  // error
  foo(new IOException)(using inlineId(???))  // error

  testPure(C()) // OK
  testPure(inlineId(C())) // OK
  testPure(identity(C())) // error, identity is not an inline function

  testPure(Pair(unsafeErasedValue[Int], unsafeErasedValue[String])) // OK
  testPure(Pair(unsafeErasedValue[Int])) // error
  testPure(Box(unsafeErasedValue[Int])) // error





