package dotty.dokka.tasty.util

import dotty.dokka.BuildInfo

object TestUtils {
  def listOurClasses(): List[String] = {
    import java.io.File
    import scala.collection.mutable.ListBuffer

    val classRoot = new File(BuildInfo.test_testcasesOutputDir)

    def go(bld: ListBuffer[String])(file: File): Unit =
      file.listFiles.foreach { f =>
        if f.isFile() then
          if f.toString.endsWith(".tasty") then bld.append(f.toString)
        else go(bld)(f)
      }

    if classRoot.isDirectory then
      val bld = new ListBuffer[String]
      go(bld)(classRoot)
      bld.result
    else
      sys.error(s"Class root could not be found: $classRoot")
  }
}
