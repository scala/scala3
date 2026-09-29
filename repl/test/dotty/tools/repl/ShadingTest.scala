package dotty.tools.repl

import scala.util.Try

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

/** Verifies that the vendored pprint/fansi/sourcecode used by the REPL are under
 *  `dotty.vendored.*`, and that their original top-level packages are not present
 *  on the REPL's classpath. This guarantees that a user depending on
 *  pprint/fansi/sourcecode (possibly at a different version) inside the REPL cannot
 *  clash with the REPL's internal copy.
 */
class ShadingTest:

  private val shadedClasses = List(
    "dotty.vendored.fansi.Str",
    "dotty.vendored.pprint.PPrinter",
    "dotty.vendored.pprint.PPrinter$Color$",
    "dotty.vendored.sourcecode.Name",
  )

  private val unshadedClasses = List(
    "fansi.Str",
    "pprint.PPrinter",
    "sourcecode.Name",
  )

  private def loads(name: String): Boolean =
    Try(Class.forName(name, false, getClass.getClassLoader)).isSuccess

  @Test def shadedClassesArePresent(): Unit =
    for name <- shadedClasses do
      assertTrue(s"expected shaded class $name to be on the REPL classpath", loads(name))

  @Test def unshadedClassesAreAbsent(): Unit =
    for name <- unshadedClasses do
      assertFalse(
        s"unshaded class $name must not leak onto the REPL classpath",
        loads(name)
      )
end ShadingTest
