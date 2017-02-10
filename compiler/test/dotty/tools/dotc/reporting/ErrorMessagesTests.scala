package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import diagnostic.messages._

import org.junit.Assert._
import org.junit.Test

class ErrorMessagesTests extends ErrorMessagesTest {
  // In the case where there are no errors, we can do "expectNoErrors" in the
  // `Report`
  @Test def noErrors =
    checkMessages("""class Foo""")
    .expectNoErrors

  @Test def typeMismatch =
    checkMessages {
      """
      |object Foo {
      |  def bar: String = 1
      |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      // Assert that we only got one error message
      assertMessageCount(1, messages)

      // Pattern match out the expected error
      val TypeMismatch(found, expected, _, _) :: Nil = messages

      // The type of the right hand side will actually be the constant 1,
      // therefore we check if it "derivesFrom"  `IntClass`
      assert(found.derivesFrom(defn.IntClass), s"found was: $found")

      // The expected type is `scala.String` which we dealias to
      // `java.lang.String` and compare with `=:=` to `defn.StringType` which
      // is a type reference to `java.lang.String`
      assert(expected.dealias =:= defn.StringType, s"expected was: $expected")
    }
}
