package dotty.tools.backend.jvm

import dotty.AsmConverters.instructionsFromMethod
import org.junit.Test
import org.junit.Assert.*

trait AnalysisBudgetBytecodeTest extends OptimizationBytecodeTest {
  def budget: Int

  override def initCtx = {
    val ctx = super.initCtx
    ctx.setSetting(ctx.settings.opt, true)
    ctx.setSetting(ctx.settings.YoptAnalysisBudget, budget)
  }

  val source =
    """final class Test {
      |  // the branches join, so the analysis has to merge (straight-line code costs no merge work)
      |  def test(x: Int, b: Boolean): Int = { val a = if (b) java.lang.Integer.valueOf(x) else java.lang.Integer.valueOf(0); a.asInstanceOf[Int] }
      |}
      |""".stripMargin

  def boxes: Boolean = {
    var found = false
    checkBCode(source) { dir =>
      val meth = getMethod(loadClassNode(lookupClass(dir, "Test.class")), "test")
      found = instructionsFromMethod(meth).exists(isCall("java/lang/Integer", "valueOf"))
    }
    found
  }
}

class AnalysisBudgetDefaultTests extends AnalysisBudgetBytecodeTest {
  def budget = 10_000_000

  @Test def boxEliminatedWithinBudget(): Unit = assertFalse(boxes)
}

class AnalysisBudgetExhaustedTests extends AnalysisBudgetBytecodeTest {
  def budget = 0

  // The producer/consumer analysis gives up, so box-unbox elimination is skipped; the method is still valid.
  @Test def boxKeptWhenBudgetExhausted(): Unit = assertTrue(boxes)
}

class AnalysisBudgetInvariantTests {
  import org.objectweb.asm.Opcodes.*
  import org.objectweb.asm.tree.{InsnNode, MethodNode}
  import dotty.tools.backend.jvm.analysis.AnalysisBudget

  def method(): MethodNode = {
    val m = new MethodNode(ACC_PUBLIC | ACC_STATIC, "f", "()V", null, null)
    m.instructions.add(new InsnNode(RETURN))
    m
  }

  @Test def rewriteAfterAnalysisIsAllowed(): Unit = {
    val m = method()
    val budget = AnalysisBudget(Int.MaxValue)
    budget.run(m, ()) {
      budget.prodConsAnalyzer(m, "C").frameAt(m.instructions.getFirst)
      m.instructions.insert(new InsnNode(NOP))
    }
  }

  @Test def nestedRunFailsLoudly(): Unit = {
    val m = method()
    val budget = AnalysisBudget(Int.MaxValue)
    val e = assertThrows(classOf[AssertionError], () => budget.run(m, ())(budget.run(m, ())(())))
    assertTrue(e.getMessage, e.getMessage.contains("not reentrant"))
    budget.run(m, ())(()) // the failed nested run did not leave the budget stuck in a run
  }

  @Test def rewriteBeforeAnalysisFailsLoudly(): Unit = {
    val m = method()
    val budget = AnalysisBudget(Int.MaxValue)
    val e = assertThrows(classOf[AssertionError], () => budget.run(m, ()) {
      m.instructions.insert(new InsnNode(NOP))
      budget.prodConsAnalyzer(m, "C").frameAt(m.instructions.getFirst)
    })
    assertTrue(e.getMessage, e.getMessage.contains("C.f()V was changed by an optimization before its analysis started"))
  }
}
