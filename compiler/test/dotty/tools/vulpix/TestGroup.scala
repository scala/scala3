package dotty.tools.vulpix

/** Test groups are used to ensure that the output of tests do not overlap.
 *
 *  A test group can be used to disambiguate outputs of tests that test the same file
 *  but with different options as shown in the following example.
 *
 *    compileFilesInDir("tests/pos", defaultOptions)(TestGroup("compileStdLib")) // will output in ./out/compileStdLib/...
 *    compileFilesInDir("tests/pos", defaultOptimised)(TestGroup("optimised/testOptimised")) // will output in ./out/optimised/testOptimised/...
 *
 *  `reportAs` lets an output-isolation subgroup contribute to its enclosing
 *  top-level progress and timing report. `reportTimings` is false for staged
 *  tests whose individual compilation steps are not independent tests.
 */
case class TestGroup(
  name: String,
  reportAs: Option[String] = None,
  reportTimings: Boolean = true,
):
  /** The top-level group used for progress and timing reports. */
  def reportingName: String = reportAs.getOrElse(name)

  /** A physically isolated subgroup with this group's reporting identity and policy. */
  def child(childName: String): TestGroup =
    TestGroup(childName, Some(reportingName), reportTimings)

  override def toString: String = name
