package dotty.tools.scaladoc

import java.io.File
import java.nio.file.Files
import org.junit.Test
import org.junit.Assert.*
import dotty.tools.scaladoc.util.IO

/** The repro from scala/scala3#22875: `A` lives in a directory reachable only through `-classpath`,
 *  which is given exactly once on the command line.
 */
class ClasspathTest:

  @Test
  def i22875(): Unit =
    val root = Files.createTempDirectory("scaladoc-i22875")
    try
      val aOutput = root.resolve("a")
      compileStage(aOutput, Nil, copyTestResource(root, "i22875", "A.scala"))

      val bOutput = root.resolve("b")
      compileStage(bOutput, Seq(aOutput), copyTestResource(root, "i22875", "B.scala"))

      val docOutput = root.resolve("doc").toFile
      Files.createDirectories(docOutput.toPath)
      val tasty = collectTastyFiles(bOutput)
      assert(tasty.nonEmpty, s"Expected .tasty files under $bOutput")

      val args = Array(
        "-project", "b",
        "-d", docOutput.toString,
        "-classpath", Seq(aOutput.toString, javaClasspath).mkString(File.pathSeparator)
      ) ++ tasty.map(_.toString)
      val reporter = (new Main).run(args)

      // the classpath is given once, so it must not be reported as re-set
      val classpathWarnings = reporter.allWarnings.map(_.message).filter(_.contains("-classpath"))
      assertEquals(
        s"Unexpected -classpath warnings:\n${classpathWarnings.mkString("\n")}",
        Nil,
        classpathWarnings
      )

      // and the classpath must still take effect: `A` is reachable only through it, so reading
      // `class B extends A` from TASTy errors out if it does not reach the compiler
      assertEquals(reporter.allErrors.map(_.message).mkString("\n"), 0, reporter.errorCount)
      assertTrue("B.html should have been rendered", new File(new File(docOutput, "b"), "B.html").exists)
    finally IO.delete(root.toFile)
