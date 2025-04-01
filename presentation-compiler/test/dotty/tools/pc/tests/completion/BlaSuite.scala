package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite
import org.junit.Test

class BlaSuite extends BaseCompletionSuite:
  @Test
  def t1 = check(
    """
    |object M:
    | val VersionRegex = "".r
    | VersionRe@@"1234"
    """.stripMargin,
    "|VersionRegex: Regex".stripMargin 
  )

  @Test
  def t2 = check(
    """
    |object M:
    | ra@@"1234"
    """.stripMargin,
    "|raw(args: Any*): String".stripMargin 
  )