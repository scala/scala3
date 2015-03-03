package dotc

import org.junit.Test
import test._

class reim_lib extends CompilerTest {

  val noCheckOptions = List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

  implicit val defaultOptions = noCheckOptions ++ List(
      "-Yno-deep-subtypes",
      "-Ycheck:resolveSuper,mixin,restoreScopes"
//  , "-Xprint:front"
  )

  val twice = List("#runs", "2", "-YnoDoubleBindings")
//  val doErase = List("-Ystop-before:terminal", "-Ylog-classpath", "-cp /home/olhotak/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.4.jar")
  val doErase = List("-Ystop-before:terminal")
  val allowDeepSubtypes = defaultOptions diff List("-Yno-deep-subtypes")

  val posDir = "./tests/pos/"
  val posSpecialDir = "./tests/pos-special/"
  val negDir = "./tests/neg/"
  val newDir = "./tests/new/"
  val dotcDir = "./src/dotty/"
  val reimDir = "./tests/reim-lib/"


  @Test def pos_imm_list = compileFile(reimDir, "scala/collection/immutable/List", doErase)
  @Test def pos_imm_traversable = compileFile(reimDir, "scala/collection/immutable/Traversable", doErase)
  @Test def pos_imm_sortedset = compileFile(reimDir, "scala/collection/immutable/SortedSet", doErase)
  @Test def pos_vlad_list = compileFile(reimDir, "LinkedList", doErase)

  @Test def pos_list_override = compileFile(reimDir, "LinkedListTest", doErase)
}
