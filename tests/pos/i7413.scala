import scala.language.implicitConversions

trait Fixture[A] extends Conversion[0, A]

trait TestFramework[A]:
  extension (testName: String) def in(test: Fixture[A] ?=> Unit): Unit = ???

trait Greeter:
  def greet(name: String): String = s"Hello $name"

case class MyFixture(name: String, greeter: Greeter)

object Test1:
  given conv: Conversion[0, Greeter]:
    def apply(x: 0): Greeter = ???
  val g: Greeter = 0

class MyTest extends TestFramework[MyFixture]:
  "say hello" in {
    assert(0.greeter.greet(0.name) == s"Hello ${0.name}")
  }

