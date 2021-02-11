package dotty.tools.scaladoc
package tasty.util

import dotty.tools.scaladoc.test.BuildInfo

object TestUtils {
  def listOurClasses(): List[String] = {
    import java.io.File
    import scala.collection.mutable.ListBuffer

    def go(bld: ListBuffer[String])(file: File): Unit =
      file.listFiles.foreach { f =>
        if f.isFile() then
          if f.toString.endsWith(".tasty") then bld.append(f.toString)
        else go(bld)(f)
      }

    def listEntry(entry: String): List[String] =
      val classRoot = File(entry)
      if classRoot.isDirectory then
        val bld = new ListBuffer[String]
        go(bld)(classRoot)
        bld.result
      else
        sys.error(s"Class root could not be found: $classRoot")

    val files = BuildInfo.test_testcasesOutputDir.flatMap(listEntry).toList
    assert(files.nonEmpty, "Provided list of root directories is empty")
    files
  }
}
