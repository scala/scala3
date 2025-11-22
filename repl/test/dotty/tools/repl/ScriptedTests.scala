package dotty.tools
package repl

import org.junit.Test

import java.io.File

final class ScriptedTests extends ReplTest:

  private def scripts(path: String): Array[File] =
      val dir = new File(path)
      assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
      dir.listFiles.filter: file =>
        val path = if file.isDirectory then file.getPath + "/" else file.getPath
        dotty.Properties.testsFilter.isEmpty || dotty.Properties.testsFilter.exists(path.contains)
  end scripts

  @Test def replTests = scripts("test-resources/repl").foreach(testFile)

  @Test def replMacrosTests = scripts("test-resources/repl-macros").foreach(testFile)

  @Test def typePrinterTests = scripts("test-resources/type-printer").foreach(testFile)

object ScriptedTests
