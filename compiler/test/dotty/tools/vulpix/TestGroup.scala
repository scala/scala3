package dotty.tools
package vulpix

/** Test groups are used to ensure that the output of tests do not overlap.
 *
 *  It can be used to disambiguate ouputs of tests that test the same file but with different options as shown in the following example.
 *    compileFilesInDir("tests/pos", defaultOptions)(TestGroup("compileStdLib")) // will output in ./out/compileStdLib/...
 *    compileFilesInDir("tests/pos", defaultOptimised)(TestGroup("optimised/testOptimised")) // will output in ./out/optimised/testOptimised/...
 */
class TestGroup(val name: String) extends AnyVal {
  override def toString: String = name
}

object TestGroup {
  def apply(name: String): TestGroup = new TestGroup(name)
}
