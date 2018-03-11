package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert._

import ReplTest._

class TypeTests extends ReplTest {
  @Test def typeOf1 = fromInitialState { implicit s =>
    compiler.typeOf("1")
            .fold(onErrors, assertEquals("Int", _))
  }

  @Test def typeOfBlock = fromInitialState { implicit s =>
    compiler.typeOf("{ /** omg omg omg */ 1 + 5; 1 }")
            .fold(onErrors,  assertEquals("Int", _))
  }

  @Test def typeOfX =
    fromInitialState { implicit s => compile("val x = 5".toParsed) }
    .andThen { implicit s =>
      compiler.typeOf("x")
              .fold(onErrors, assertEquals("Int", _))
    }
}
