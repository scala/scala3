package dotty.tools.vulpix

/** Test groups are used to ensure that the output of tests do not overlap.
 *
 *  A test group can be used to disambiguate outputs of tests that test the same file
 *  but with different options as shown in the following example.
 *
 *    compileFilesInDir("tests/pos", defaultOptions)(TestGroup("compileStdLib")) // will output in ./out/compileStdLib/...
 *    compileFilesInDir("tests/pos", defaultOptimised)(TestGroup("optimised/testOptimised")) // will output in ./out/optimised/testOptimised/...
 */
case class TestGroup(name: String):
  override def toString: String = name