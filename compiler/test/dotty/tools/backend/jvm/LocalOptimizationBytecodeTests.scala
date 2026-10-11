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

  // Ensure writing `case None` is as efficient as `case _` after `case Some`
  // This matters not only to avoid a null check in itself, but because the function may then be small enough to be inlineable
  @Test def patternMatchingOption =
    assertEquivalence(
      "if x.isInstanceOf[Some[Int]] then x.asInstanceOf[Some[Int]].value else 0",
      "x match { case Some(v) => v; case None => 0 }",
      params = List("x: Option[Int]"),
      returnType = "Int"
    )

  // Same but for List: `case Nil` should be as efficient as `case _` after `case _ :: _`
  @Test def patternMatchingList =
    assertEquivalence(
      "if x.isInstanceOf[`::`[Int]] then x.asInstanceOf[`::`[Int]].head else 0",
      "x match { case v :: _ => v; case Nil => 0 }",
      params = List("x: List[Int]"),
      returnType = "Int"
    )

  // Similar as the above, inspired by a compiler function
  @Test def patternMatchingLazyOfT =
    assertEquivalence(
      "if x.isInstanceOf[Lazy[?]] then x.asInstanceOf[Lazy[T]].value else x.asInstanceOf[T]",
      "x match { case l: Lazy[T] @unchecked => l.value; case t: T @unchecked => t }",
      extraSource = "class Lazy[T](val value: T)",
      params = List("x: Lazy[T] | T"),
      genericParams = List("T"),
      returnType = "T"
    )
}
