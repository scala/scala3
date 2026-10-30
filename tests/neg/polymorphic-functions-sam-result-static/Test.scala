// The JVM runs the static initializer of an interface with default methods when it
// creates the first instance of a class that implements it. So the body of a polymorphic
// function literal, which is created once and shared by all type applications, cannot be
// converted to a SAM type with a static initializer.
import scala.annotation.static

trait ScalaStatic[A]:
  def apply(x: A): A
  def get: Int = ScalaStatic.s
object ScalaStatic:
  @static val s: Int = "static".length

trait ScalaStaticLiteral[A]:
  def apply(x: A): A
  def get: Int = ScalaStaticLiteral.s
object ScalaStaticLiteral:
  @static val s: 1 = { println("init"); 1 }

def test =
  val f1: [A] => JavaStatic[A] = [A] => (x: A) => x // error
  val f2: [A] => ScalaStatic[A] = [A] => (x: A) => x // error
  val f3: [A] => ScalaStaticLiteral[A] = [A] => (x: A) => x // error
