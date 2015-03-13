package dotc

import org.junit.Test
import test._
import java.io.File

class tests extends CompilerTest {

  val noCheckOptions = List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

  implicit val defaultOptions = noCheckOptions ++ List(
      "-Yno-deep-subtypes",
      "-Ycheck:tailrec,resolveSuper,mixin,restoreScopes",
      "-d", "./out/"
  )

  val failedbyName = List("-Ystop-before:collectEntryPoints") // #288
  val failedUnderscore = List("-Ystop-before:collectEntryPoints") // #289

  val failedOther = List("-Ystop-before:collectEntryPoints") // some non-obvious reason. need to look deeper
  val twice = List("#runs", "2", "-YnoDoubleBindings")

  val allowDeepSubtypes = defaultOptions diff List("-Yno-deep-subtypes")

  val dotcDir = "./src/dotty/"

  @Test def dotc = compileDir(dotcDir + "tools/dotc", failedOther)(allowDeepSubtypes)
  @Test def dotc_ast = compileDir(dotcDir + "tools/dotc/ast", failedOther) // similar to dotc_config
  @Test def dotc_config = compileDir(dotcDir + "tools/dotc/config", failedOther) // seems to mess up stack frames
  @Test def dotc_core = compileDir(dotcDir + "tools/dotc/core", failedUnderscore)(allowDeepSubtypes)
  // fails due to This refference to a non-eclosing class. Need to check

  @Test def dotc_core_pickling = compileDir(dotcDir + "tools/dotc/core/pickling", failedOther)(allowDeepSubtypes) // Cannot emit primitive conversion from V to Z

  @Test def dotc_transform = compileDir(dotcDir + "tools/dotc/transform", failedbyName)

  @Test def dotc_parsing = compileDir(dotcDir + "tools/dotc/parsing", failedOther)
    //  Expected primitive types I - Ljava/lang/Object
    //  Tried to return an object where expected type was Integer
  @Test def dotc_printing = compileDir(dotcDir + "tools/dotc/printing", failedOther)
  @Test def dotc_reporting = compileDir(dotcDir + "tools/dotc/reporting", twice)
  @Test def dotc_typer = compileDir(dotcDir + "tools/dotc/typer", failedOther) // similar to dotc_config
  //@Test def dotc_util = compileDir(dotcDir + "tools/dotc/util") //fails inside ExtensionMethods with ClassCastException
  @Test def tools_io = compileDir(dotcDir + "tools/io", failedOther) // similar to dotc_config

  //@Test def tools = compileDir(dotcDir + "tools", "-deep" :: Nil)(allowDeepSubtypes)

  @Test def testNonCyclic = compileArgs(Array(
      dotcDir + "tools/dotc/CompilationUnit.scala",
      dotcDir + "tools/dotc/core/Types.scala",
      dotcDir + "tools/dotc/ast/Trees.scala",
      failedUnderscore.head,
      "-Xprompt",
      "#runs", "2"))

  @Test def testIssue_34 = compileArgs(Array(
      dotcDir + "tools/dotc/config/Properties.scala",
      dotcDir + "tools/dotc/config/PathResolver.scala",
      //"-Ylog:frontend",
      "-Xprompt",
      "#runs", "2"))

  //@Test def dotc_compilercommand = compileFile(dotcDir + "tools/dotc/config/", "CompilerCommand")


  // =========== PARTEST MIGRATION NOTES =========
  // Most pos and neg tests have been moved to tests/partest-tests/, run with
  // sbt test or test-only partest

  // =========== non-partest-able tests ==========
  // Partest checks that mixed java and scala sources compile individually, but
  // here the scala source depends on the java source.
  val nonPPosDir = "./tests/non-partest-tests/pos/"
  @Test def nonpartest_pos_javaOverride() = compileDir(nonPPosDir + "java-override")
  @Test def nonpartest_pos_java_all = compileFiles(nonPPosDir + "java-interop/")

  // =========== tests failing both here and in partest ========
  // copy back to ./tests/partest-tests/pos/ once these tests are fixed
  val failingPosDir = "./tests/failing/pos/"

  // failing with "error: Error while emitting typers.scala
  // assertion failed: Trying to access the this of another class"
  @Test def pos_typers() = compileFile(failingPosDir, "typers")

  // error: type p$C$$A overrides nothing: object `package` extends C[String]
  @Test def pos_packageObj = compileFile(failingPosDir, "i0239")
  @Test def pos_packageObj2 = compileFile(failingPosDir, "i239-packageObj")


  // =========== partest flags ============
  // default options are read from __defaultFlags.flags in the same directory
  // as the test unless there's a test-specific .flags file with the same name
  // as the test target (in which case defaultFlags is ignored).

  // the pos tests that were compiled with failedOther above have been moved to
  // pos-failedOther with corresponding defaultFlags

  // removed unknown -Xfatal-warnings flag from:
  // pos/tailcall/t4649.flags (becomes empty, so deleted)
  // pos/tailcall/t6891.flags
  // pos-failedOther/t2799.flags

  // pos/nullarify added flags file for additional "-Ycheck:nullarify"

  // The following files need deep subtypes and have thus a flags file that
  // contains all defaultFlags except for "-Yno-deep-subtypes":
  // pos/t2613

  // neg/t0625 used noCheckOptions from above. Previous comment: "-Ycheck fails
  // because there are structural types involving higher-kinded types. these
  // are illegal, but are tested only later."

  // added additional -Ystop-before:terminal to
  // pos/HelloWorld
  // pos/Labels

  // ========== partest groups ===========
  // The following tests originally have files in different groups (name ending
  // in _1 resp. _2), but the second compilation doesn't get the classes from
  // the first (because there are no class files yet?). Quick fix: removed
  // grouping by removing the underscore.
  // pos-failedOther/t1029
  // pos-failedOther/t1942
  // pos-failedOther/t2726
  // pos-failedOther/t2741
  // pos-failedOther/t2764

  // ========== neg tests ================
  // I added error number testing, for each neg test listed here with an
  // expected number of errors I created a target.nerr file with the number
  // that gets checked.

  // The neg directory contains additional tests that weren't listed here,
  // they all pass (=compilation fails) except for the following tests that
  // were moved to "./tests/failing/neg/":
  // t0625.scala [expected compilation failure, but compilation passed]
  // t0654.scala [expected compilation failure, but compilation passed]
  // t1164.scala [expected compilation failure, but compilation passed]
  // tailcall [throws an exception while typechecking]

}
