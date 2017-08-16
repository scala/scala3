package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert._

class TypeTests extends ReplTest {
  @Test def typeOf1 = withState {
    compiler.typeOf("1").fold(onErrors, assertEquals("Int", _))
  }

  @Test def typeOfBlock = withState {
    compiler.typeOf("{ /** omg omg omg */ 1 + 5; 1 }")
            .fold(onErrors,  assertEquals("Int", _))
  }

  @Test def typeOfX =
    fromState(withState(compile("val x = 5"))) {
      compiler.typeOf("x")
              .fold(onErrors, assertEquals("Int", _))
    }
}
