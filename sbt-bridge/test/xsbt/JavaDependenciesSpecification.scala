package xsbt

import dotty.tools.xsbt.CompilerBridge
import sbt.io.IO
import xsbti.*
import xsbti.api.DependencyContext.*
import xsbti.compile.SingleOutput

import java.io.File
import org.junit.Test
import org.junit.Assert.*

class JavaDependenciesSpecification:

  @Test
  def javaDependenciesUnderJavaTasty(): Unit =
    val temp = IO.createTemporaryDirectory
    val srcA = new File(temp, "A.scala")
    IO.write(srcA, "package p; class A")
    val srcB = new File(temp, "B.java")
    IO.write(srcB, "package p; public class B extends A { public A a() { return null; } }")

    val callback = new TestCallback
    new CompilerBridge().run(
      Array(srcA, srcB).map(f => new TestVirtualFile(f.toPath)),
      new TestDependencyChanges,
      Array("-usejavacp", "-d", temp.getAbsolutePath, "-Yforce-sbt-phases", "-Xjava-tasty"),
      new SingleOutput { def getOutputDirectory(): File = temp },
      callback,
      new TestReporter,
      new TestCompileProgress,
      new TestLogger,
    )

    assertEquals(
      Set(("p.A", "p.B", DependencyByInheritance), ("p.A", "p.B", DependencyByMemberRef)),
      callback.classDependencies.toSet)
