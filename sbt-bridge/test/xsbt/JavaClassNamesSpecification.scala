package xsbt

import dotty.tools.xsbt.CompilerBridge
import sbt.io.IO
import xsbti.*
import xsbti.compile.SingleOutput

import java.io.File
import org.junit.Test
import org.junit.Assert.*

class JavaClassNamesSpecification:

  @Test
  def javaClassInEmptyPackage(): Unit =
    val temp = IO.createTemporaryDirectory
    val src = new File(temp, "B.java")
    IO.write(src, "public class B { public static class Inner {} }")

    val callback = new TestCallback
    new CompilerBridge().run(
      Array(new TestVirtualFile(src.toPath)),
      new TestDependencyChanges,
      Array("-usejavacp", "-d", temp.getAbsolutePath, "-Yforce-sbt-phases", "-Xjava-tasty"),
      new SingleOutput { def getOutputDirectory(): File = temp },
      callback,
      new TestReporter,
      new TestCompileProgress,
      new TestLogger,
    )

    assertEquals(Set("B", "B.Inner"), callback.apis.values.flatten.map(_.name).toSet)
