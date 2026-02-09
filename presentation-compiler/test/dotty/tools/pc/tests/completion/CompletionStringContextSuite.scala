package dotty.tools.pc.tests.completion

import dotty.tools.dotc.core.Types.ThisType.raw
import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionStringContextSuite extends BaseCompletionSuite:
  @Test
  def inScopeSymbol = check(
    """
    |object M:
    | val VersionRegex = "".r
    | VersionRe@@"1234"
    """.stripMargin,
    "|VersionRegex: Regex".stripMargin
  )

  @Test
  def workspaceSymbol = check(
    """
    |object M:
    | ListBuf@@"1234" 
    """.stripMargin,
    """
    |ListBuffer[A](elems: A*): ListBuffer[A] - scala.collection.mutable
    |new ListBuffer[A]: ListBuffer[A] - scala.collection.mutable
    |ListBuffer - scala.collection.mutable
    |""".stripMargin
  )

  @Test
  def providedSymbol = check(
    """
    |object M:
    | ra@@"1234"
    """.stripMargin,
    "|raw(args: Any*): String".stripMargin
  )

  // bellow are tests of edits
  @Test
  def editTest1 = checkEdit(
    """
    |object M:
    | ra@@"1234"
    """.stripMargin,
    """
    |object M:
    | raw"1234"
    |""".stripMargin
  )

  @Test
  def editTest2 = checkEdit(
    """
    |object M:
    | printl@@"1234"
    """.stripMargin,
    """
    |object M:
    | println"1234"
    |""".stripMargin,
    assertSingleItem = false
  )

  @Test
  def editTest3 = checkEdit(
    """
    |object M:
    | def select(s: String): String = s
    | selec@@"1234"
    """.stripMargin,
    """
    |object M:
    | def select(s: String): String = s
    | select"1234"
    |""".stripMargin,
    assertSingleItem = false
  )
