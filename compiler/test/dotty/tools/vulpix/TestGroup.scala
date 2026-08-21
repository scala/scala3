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
 *  top-level progress and timing report. `reportTimings` excludes staged tests.
 */
case class TestGroup(name: String, reportAs: Option[String] = None, reportTimings: Boolean = true):
  def reportingName: String = reportAs.getOrElse(name)

  def child(childName: String): TestGroup =
    copy(name = childName, reportAs = Some(reportingName))

  override def toString: String = name
