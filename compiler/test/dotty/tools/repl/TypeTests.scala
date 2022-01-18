package dotty.tools.repl

import org.junit.Assert._
import org.junit.Test

class TypeTests extends ReplTest {
  @Test def typeOf1 = initially {
    run(":type 1")
    assertEquals("Int", storedOutput().trim)
  }

  @Test def typeOfBlock = initially {
    run(":type { /** omg omg omg */ 1 + 5; 1 }")
    assertEquals("Int", storedOutput().trim)
  }

  @Test def typeOfX =
    initially {
      run("val x = 5")
    } andThen {
      storedOutput() // discard output
      run(":type x")
      assertEquals("Int", storedOutput().trim)
    }

  @Test def typeOfEmpty = initially {
    run(":type")
    assertEquals(":type <expression>", storedOutput().trim)
  }
}
