package xsbt

import org.junit.Test
import org.junit.Assert._

/**
 * End-to-end recompilation-decision tests for the pattern-matching
 * undercompilation fixes in ExtractDependencies.
 *
 * Each test compiles an upstream source (v1), a downstream source that
 * pattern-matches on it, then compiles a modified upstream (v2) and asks
 * the `IncrementalCompileSimulator` whether downstream *would* be
 * recompiled — using the real Zinc name-hashing algorithm.
 */
class IncrementalCompileSimulatorSpecification:
  private val sim = new IncrementalCompileSimulator

  // ── case class: field type change (issue #26231) ─────────────────────────

  @Test
  def caseClassFieldTypeChange_invalidatesPatternMatch(): Unit =
    val upstream1   = "case class C(x: Int)"
    val upstream2   = "case class C(x: String)"
    val downstream  =
      """|object Test {
         |  def f(c: C): Any = c match { case C(x) => x }
         |}""".stripMargin
    assertTrue(
      "changing a case class field type must invalidate a file that pattern-matches on it",
      sim.wouldRecompile(upstream1, downstream, upstream2)
    )

  // ── case class: field added ───────────────────────────────────────────────

  @Test
  def caseClassFieldAdded_invalidatesPatternMatch(): Unit =
    val upstream1   = "case class C(x: Int)"
    val upstream2   = "case class C(x: Int, y: String)"
    val downstream  =
      """|object Test {
         |  def f(c: C): Any = c match { case C(x) => x }
         |}""".stripMargin
    assertTrue(
      "adding a case class field must invalidate a file that pattern-matches on it",
      sim.wouldRecompile(upstream1, downstream, upstream2)
    )

  // ── unapplySeq added beside unapply ──────────────────────────────────────

  @Test
  def unapplySeqAdded_invalidatesPatternMatch(): Unit =
    val upstream1 =
      """|object E {
         |  def unapply(x: String): Option[Int] = Some(x.length)
         |}""".stripMargin
    val upstream2 =
      """|object E {
         |  def unapply(x: String): Option[Int]        = Some(x.length)
         |  def unapplySeq(x: String): Option[Seq[Char]] = Some(x.toSeq)
         |}""".stripMargin
    val downstream =
      """|object Test {
         |  def f(s: String): Any = s match { case E(n) => n }
         |}""".stripMargin
    assertTrue(
      "adding unapplySeq alongside an existing unapply must invalidate a file that matched via unapply",
      sim.wouldRecompile(upstream1, downstream, upstream2)
    )

  // ── unapply added beside unapplySeq ──────────────────────────────────────

  @Test
  def unapplyAdded_invalidatesPatternMatch(): Unit =
    val upstream1 =
      """|object E {
         |  def unapplySeq(x: String): Option[Seq[Char]] = Some(x.toSeq)
         |}""".stripMargin
    val upstream2 =
      """|object E {
         |  def unapply(x: String): Option[Int]          = Some(x.length)
         |  def unapplySeq(x: String): Option[Seq[Char]] = Some(x.toSeq)
         |}""".stripMargin
    val downstream =
      """|object Test {
         |  def f(s: String): Any = s match { case E(a, b) => (a, b) }
         |}""".stripMargin
    assertTrue(
      "adding unapply alongside an existing unapplySeq must invalidate a file that matched via unapplySeq",
      sim.wouldRecompile(upstream1, downstream, upstream2)
    )

  // ── custom product extractor (_1/_2 protocol) ────────────────────────────
  //
  // A non-synthetic unapply that returns a plain class with _1, _2 val members
  // takes the non-case-class product path in ExtractDependencies.  That path
  // records _1, _2, … via addMemberRefDependency, and _N+1 (one past the
  // current arity) as a sentinel via addUsedRawName, so that:
  //   - return-type changes on any existing selector trigger recompilation
  //   - removing a selector triggers recompilation
  //   - adding a new selector at or below the sentinel position triggers recompilation

  private val productExtractorUpstream1 =
    """|class R2(val _1: Int, val _2: String) {
       |  def isEmpty: Boolean = false
       |  def get: R2 = this
       |}
       |object E { def unapply(s: String): R2 = new R2(s.length, s) }""".stripMargin

  private val productExtractorDownstream =
    """|object Test {
       |  def f(s: String): Any = s match { case E(a, b) => (a, b) }
       |}""".stripMargin

  @Test
  def productExtractor_selectorTypeChange_invalidatesPatternMatch(): Unit =
    val upstream2 =
      """|class R2(val _1: Long, val _2: String) {
         |  def isEmpty: Boolean = false
         |  def get: R2 = this
         |}
         |object E { def unapply(s: String): R2 = new R2(s.length.toLong, s) }""".stripMargin
    assertTrue(
      "changing the type of a product selector must invalidate a file that uses it in a pattern",
      sim.wouldRecompile(productExtractorUpstream1, productExtractorDownstream, upstream2)
    )

  @Test
  def productExtractor_selectorRemoved_invalidatesPatternMatch(): Unit =
    val upstream2 =
      """|class R2(val _1: Int) {
         |  def isEmpty: Boolean = false
         |  def get: R2 = this
         |}
         |object E { def unapply(s: String): R2 = new R2(s.length) }""".stripMargin
    assertTrue(
      "removing a product selector must invalidate a file whose pattern binds that position",
      sim.wouldRecompile(productExtractorUpstream1, productExtractorDownstream, upstream2)
    )

  @Test
  def productExtractor_selectorAdded_invalidatesPatternMatch(): Unit =
    val upstream2 =
      """|class R2(val _1: Int, val _2: String, val _3: Boolean) {
         |  def isEmpty: Boolean = false
         |  def get: R2 = this
         |}
         |object E { def unapply(s: String): R2 = new R2(s.length, s, s.nonEmpty) }""".stripMargin
    assertTrue(
      "adding a product selector (_3) must invalidate a file that matched at arity 2 (recorded _3 as sentinel)",
      sim.wouldRecompile(productExtractorUpstream1, productExtractorDownstream, upstream2)
    )

  // ── case class loses its case modifier ───────────────────────────────────

  @Test
  def caseClassLosesCaseFlag_invalidatesPatternMatch(): Unit =
    val upstream1  = "case class C(x: Int)"
    // The companion is kept explicit so both versions have ("C", Module) and
    // the test exercises memberChangedNames rather than the whole-class-disappears
    // arm of changedNamesBetween.
    //
    // This test passes even without the ExtractDependencies fix because `unapply`
    // is recorded as a used name by the ordinary Select traversal of the UnApply
    // node's `fun` sub-tree.  It is a regression guard for the overall mechanism,
    // not a test of our new code paths.
    val upstream2  =
      """|class C(val x: Int)
         |object C""".stripMargin
    val downstream =
      """|object Test {
         |  def f(c: C): Any = c match { case C(x) => x }
         |}""".stripMargin
    assertTrue(
      "removing the case modifier (and thus unapply) must invalidate a file that pattern-matches on it",
      sim.wouldRecompile(upstream1, downstream, upstream2)
    )

  // ── sanity: identical upstream must NOT invalidate ────────────────────────

  @Test
  def noChange_doesNotInvalidate(): Unit =
    val upstream    = "case class C(x: Int)"
    val downstream  =
      """|object Test {
         |  def f(c: C): Any = c match { case C(x) => x }
         |}""".stripMargin
    assertFalse(
      "an unchanged upstream must not trigger recompilation",
      sim.wouldRecompile(upstream, downstream, upstream)
    )
