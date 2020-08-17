package dotty.tools.repl

import org.junit.Assert._
import org.junit.Test

class TypeTests extends ReplTest {
  @Test def typeOf1 = fromInitialState { implicit s =>
    run(":type 1")
    assertEquals("Int", storedOutput().trim)
  }

  @Test def typeOfBlock = fromInitialState { implicit s =>
    run(":type { /** omg omg omg */ 1 + 5; 1 }")
    assertEquals("Int", storedOutput().trim)
  }

  @Test def typeOfX =
    fromInitialState { implicit s => run("val x = 5") }
    .andThen { implicit s =>
      storedOutput() // discard output
      run(":type x")
      assertEquals("Int", storedOutput().trim)
    }

  @Test def typeOfEmpty = fromInitialState { implicit s =>
    run(":type")
    assertEquals(":type <expression>", storedOutput().trim)
  }
}
