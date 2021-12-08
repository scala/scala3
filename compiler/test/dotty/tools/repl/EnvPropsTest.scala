package dotty.tools.repl

import org.junit.Assert._
import org.junit.Test

class EnvPropsTest extends ReplTest(options = Array("-J-Dkey=value", "-Dkey2=value2")) {
  @Test def fromDashJDashD = fromInitialState { implicit s =>
    run("util.Properties.propOrNull(\"key\")")
    assertTrue(storedOutput().contains("val res0: String = value"))
  }

  @Test def fromDashD = fromInitialState { implicit s =>
    run("util.Properties.propOrNull(\"key2\")")
    assertTrue(storedOutput().contains("val res0: String = value2"))
  }
}

