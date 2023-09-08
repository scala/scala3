package dotty.tools.repl

import org.junit.Assert._
import org.junit.Test

class JavaDefinedTests extends ReplTest {
  @Test def typeOfJavaDefinedString = initially {
    run("String")
    assertTrue(storedOutput().contains("Java defined class String is not a value"))
  }

  @Test def typeOfJavaDefinedClass = initially {
    run("Class")
    assertTrue(storedOutput().contains("Java defined class Class is not a value"))
  }
}
