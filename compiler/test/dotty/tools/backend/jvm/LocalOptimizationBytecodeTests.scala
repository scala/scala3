package dotty.tools.backend.jvm

import org.junit.Test

class LocalOptimizationBytecodeTests extends OptimizationBytecodeTest {
  override def initCtx = {
    val ctx = super.initCtx
    ctx.setSetting(ctx.settings.opt, true)
  }

  // Ensures we recognize Predef$ boxing methods
  @Test def boolean2BooleanIsNonNull =
    assertEquivalence(
      "true",
      "inline def foo(x: java.lang.Boolean): Boolean = { x != null }; foo(b)",
      params = List("b: Boolean"),
      returnType = "Boolean"
    )
}
