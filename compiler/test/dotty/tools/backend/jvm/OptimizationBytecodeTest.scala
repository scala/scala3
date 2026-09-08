package dotty.tools.backend.jvm

import dotty.{AsmConverters, DottyBytecodeTest}
import dotty.AsmConverters.instructionsFromMethod

trait OptimizationBytecodeTest extends DottyBytecodeTest {
  def escapeSource(fullSource: String): String = {
    // for easier debugging via cleaner logs, only generate the escape method if it's actually used
    if fullSource.contains("escape")
    then
      """
        |// Because our elimination of load-store pairs isn't amazing, we define escaping as a purely static function so there's no module/this load
        |// The point of the tests with escaping is not to test exactly how escaping itself is compiled
        |object escape { @noinline @annotation.static def apply(a: Any): Unit = ??? }
        |class escape { }
        |""".stripMargin
    else
      ""
  }

  def assertEquivalence(expectedSource: String, actualSource: String, params: List[String] = Nil, extraMemberSources: List[String] = Nil, returnType: String = "Int"): Unit = {
    val source =
      f"""
         |${escapeSource(expectedSource + actualSource + extraMemberSources.mkString("\n"))}
         |final class Test {
         |  ${extraMemberSources.mkString("\n  ")}
         |  def actual(${params.mkString(", ")}): $returnType = { $actualSource }
         |  def expected(${params.mkString(", ")}): $returnType = { $expectedSource }
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = lookupClass(dir, "Test.class")
      val clsNode = loadClassNode(clsIn)
      val meth1 = getMethod(clsNode, "actual")
      val meth2 = getMethod(clsNode, "expected")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "code was not properly optimized\n" +
          diffInstructions(instructions1, instructions2))
    }
  }

  def assertCalls(allowedCalls: (String, String) => Boolean, body: String, params: List[String] = Nil, extraMemberSources: List[String] = Nil, returnType: String = "Int"): Unit =
    val source =
      f"""
         |${escapeSource(body + extraMemberSources.mkString("\n"))}
         |final class Test {
         |  ${extraMemberSources.mkString("\n  ")}
         |  def test(${params.mkString(", ")}): $returnType = { $body }
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = lookupClass(dir, "Test.class")
      val clsNode = loadClassNode(clsIn)
      val meth = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(meth)
      for instr <- instructions do instr match {
        case AsmConverters.Invoke(_, owner, name, _, _) => assert(allowedCalls(owner, name), s"Found invoke to $owner.$name in:\n${instructions.mkString("\n")}")
        case AsmConverters.InvokeDynamic(_, _, _, bsm, _) => assert(allowedCalls(bsm.owner, bsm.name), s"Found dynamic invoke to ${bsm.owner}.${bsm.name} in:\n${instructions.mkString("\n")}")
        case _ => ()
      }
    }

  object Calls {
    def none(clazz: String, meth: String): Boolean = false
    def noneToClasses(disallowedClasses: String*)(clazz: String, meth: String): Boolean = disallowedClasses.forall(c => !clazz.contains(c))
    def noBoxing: (String, String) => Boolean = noneToClasses("BoxesRunTime")
    def noneExcept(allowedCalls: String*)(clazz: String, meth: String): Boolean = allowedCalls.contains(meth)
  }

  def isCall(cls: String, meth: String)(i: AsmConverters.Instruction) = i match
    case inv: AsmConverters.Invoke => inv.owner == cls && inv.name == meth
    case _ => false

}
