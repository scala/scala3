package dotty.partest

/** Dotty Partest runs all tests in the provided testDirs located under
  * testRoot. There can be several directories with pos resp. neg tests, as
  * long as the prefix is pos/neg.
  *
  * Each testDir can also have a __defaultFlags.flags file, which provides
  * compiler flags and is used unless there's a specific flags file (e.g. for
  * test pos/A.scala, if there's a pos/A.flags file those flags are used,
  * otherwise pos/__defaultFlags.flags are used if the file exists).
  */
object DottyPartestConfig {
  val testRoot = "./tests/partest-tests"
  val testDirs = Array("new", "pos", "pos-failedOther", "neg")

  // Tests finish faster when running in parallel, but console output is
  // out of order and sometimes the compiler crashes
  val runTestsInParallel = false
}
