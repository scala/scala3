package dotty.tools.dotc.coverage

import org.junit.Test
import org.junit.Assert.*

import java.io.StringWriter

class SerializerTests {
  @Test
  def basicStatementRoundtrips(): Unit =
    assertRoundtrips(Statement(
      location = Location(
        packageName = "my.package",
        className = "MyClass",
        fullClassName = "my.package.MyClass",
        classType = "Class",
        methodName = "meth",
        sourcePath = "somewhere.scala"
      ),
      id = 1,
      start = 10,
      end = 20,
      line = 2,
      desc = "desc",
      symbolName = "sym",
      treeName = "tree",
      branch = false,
      ignored = true
    ))

  @Test
  def statementWithCharsNeedingEscapeRoundtrips(): Unit =
    assertRoundtrips(Statement(
      location = Location(
        packageName = "my\npackage\\ with a weird\fname",
        className = "My\rClass",
        fullClassName = "my\npackage\\ with a weird\fname.My\rClass",
        classType = "Class",
        methodName = "meth\tod",
        sourcePath = "/tmp/\nsomewhere\\.scala"
      ),
      id = 123,
      start = 456,
      end = 789,
      line = 321,
      desc = "desc\r\nxxx",
      symbolName = "sym\nbo\\l",
      treeName = "$tree",
      branch = true
    ))

  private def assertRoundtrips(stmt: Statement): Unit = {
    val cov = new Coverage()
    cov.addStatement(stmt)

    val writer = new StringWriter()
    Serializer.serialize(cov, writer)

    val result = Serializer.deserialize(writer.toString.split('\n').iterator)
    assertEquals(1, result.statements.size)
    assertEquals(stmt, result.statements.head)
  }
}
