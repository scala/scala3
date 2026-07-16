package dotty.tools.dotc

import java.io.{PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import javax.tools.{DiagnosticCollector, JavaFileObject, ToolProvider}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.junit.{Ignore, Test}
import org.junit.Assert.*

import dotty.tools.deleteDirectory
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import dotty.tools.vulpix.TestConfiguration

/** Tests that the compiler produces byte-for-byte identical class files and
 *  TASTy when the same sources are compiled in a different order, or when
 *  subsets are recompiled separately against the class files of a previous
 *  run.
 *
 *  Ported from Scala 2's `scala.tools.nsc.DeterminismTest`, part of the
 *  reproducible-builds work tracked in scala/scala-dev#405 and, for Scala 3,
 *  in scala/scala3#7661.
 *
 *  Known limitation: all compilations run in-process in one JVM, which shares
 *  the JVM-global name-interning table (`Names.nameTable`) between the
 *  reference compile and every recompile. `SimpleName.hashCode` is the name's
 *  offset in that table, so hash-ordered iteration over names is identical
 *  for all compiles in this JVM even when two separate `scalac` invocations
 *  (fresh tables, different interning order) would diverge. Cross-JVM
 *  run-to-run non-determinism as reported in scala/scala3#22181 therefore
 *  cannot be caught here; it would need a harness that forks compiler JVMs.
 */
class DeterminismTest {
  private val tester = new DeterminismTester
  import tester.{source, test}

  @Test def testSameSourcesTwice(): Unit = {
    // Recompiling identical sources over a previous output must be a no-op,
    // also for a single larger file (scala/scala3#22181).
    val sb = new StringBuilder("package demo\n\nclass Big {\n")
    for i <- 1 to 100 do
      sb ++= s"""|  def m$i(x: Int): Any = {
                 |    def local(y: Int) = y + $i
                 |    val fn = (z: Int) => local(z)
                 |    (${i % 7}: Any) match {
                 |      case n: Int if n > 3 => Some(fn(n)).map(v => v + x)
                 |      case _ => None
                 |    }
                 |  }
                 |""".stripMargin
    sb ++= "}\n"
    def code = List(source("big.scala", sb.toString))
    test(List(code))
  }

  @Test def testLambdaLift(): Unit = {
    def code = List(
      source("a.scala",
        """package demo
          |
          |class a {
          |  def x = {
          |    def local = "x"
          |  }
          |  def y = {
          |    def local = "y"
          |  }
          |}
          |""".stripMargin),
      source("b.scala",
        """package demo
          |
          |class b {
          |  def test(): Unit = {
          |    new a().y
          |  }
          |}
          |""".stripMargin)
    )
    test(List(code))
  }

  @Test def testTyperFreshName(): Unit = {
    def code = List(
      source("a.scala",
        """package demo
          |
          |class a {
          |  def x = {
          |    { case x if "".isEmpty => "" }: PartialFunction[Any, Any]
          |  }
          |  def y = {
          |    { case x if "".isEmpty => "" }: PartialFunction[Any, Any]
          |  }
          |}
          |""".stripMargin),
      source("b.scala",
        """package demo
          |
          |class b {
          |  def test(): Unit = {
          |    new a().y
          |  }
          |}
          |""".stripMargin)
    )
    test(List(code))
  }

  /** Scala 3 analog of Scala 2's `testMacroFreshName`: inline expansion
   *  introduces compiler-generated bindings at each call site.
   */
  @Test def testInlineFreshName(): Unit = {
    val inlineCode = source("m.scala",
      """package demo
        |
        |object M {
        |  inline def m(inline cond: Boolean)(x: => Int): Int = {
        |    val tmp = if cond then x + x else 0
        |    tmp
        |  }
        |}
        |""".stripMargin)
    def code = List(
      source("a.scala",
        """package demo
          |
          |class a {
          |  def test: Int = M.m(true)(1) + M.m(false)(2)
          |}
          |""".stripMargin),
      source("b.scala",
        """package demo
          |
          |class b {
          |  def test: Int = M.m(true)(3)
          |}
          |""".stripMargin)
    )
    test(List(List(inlineCode), code))
  }

  @Test def testRefinementTypeOverride(): Unit = {
    def code = List(
      source("a.scala",
        """class Global
          |trait Analyzer extends StdAttachments {
          |  val global: Global
          |}
          |trait Context {
          |  val universe: Global
          |}
          |
          |trait StdAttachments {
          |  self: Analyzer =>
          |
          |  type UnaffiliatedMacroContext = Context
          |  type MacroContext = UnaffiliatedMacroContext { val universe: self.global.type }
          |}
          |""".stripMargin),
      source("b.scala",
        """class Macros {
          |  self: Analyzer =>
          |  def foo = List.apply[MacroContext]()
          |}
          |""".stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotations1(): Unit = {
    def code = List(
      source("a.scala",
        """class Annot1(s: String) extends scala.annotation.StaticAnnotation
          |class Annot2(s: Class[?]) extends scala.annotation.StaticAnnotation
          |""".stripMargin),
      source("b.scala",
        """@Annot1("foo")
          |@Annot2(classOf[AnyRef])
          |class Test
          |""".stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotationsJava(): Unit = {
    def code = List(
      source("Annot1.java",
        """import java.lang.annotation.*;
          |@Retention(RetentionPolicy.RUNTIME)
          |@Target(ElementType.TYPE)
          |@Inherited
          |@interface Annot1 { String value() default ""; }
          |
          |@Retention(RetentionPolicy.RUNTIME)
          |@Target(ElementType.TYPE)
          |@Inherited
          |@interface Annot2 { Class value(); }
          |""".stripMargin),
      source("b.scala",
        """@Annot1("foo") @Annot2(classOf[AnyRef]) class Test
          |""".stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotationsJavaRepeatable(): Unit = {
    val javaAnnots = source("Annot1.java",
      """import java.lang.annotation.*;
        |@Repeatable(Annot1.Container.class)
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.TYPE)
        |@interface Annot1 { String value() default "";
        |
        |    @Retention(RetentionPolicy.RUNTIME)
        |    @Target(ElementType.TYPE)
        |    public static @interface Container {
        |        Annot1[] value();
        |    }
        |}
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.TYPE)
        |@Inherited
        |@interface Annot2 { Class value(); }
        |""".stripMargin)
    def code = List(
      source("dummy.scala", ""),
      source("b.scala",
        """@Annot1("foo") @Annot2(classOf[String]) @Annot1("bar") class Test
          |""".stripMargin)
    )
    test(List(javaAnnots) :: code :: Nil)
  }

  @Test def testPackedType(): Unit = {
    def code = List(
      source("a.scala",
        """class C {
          |  def foo = { object A; object B; object C; object D; object E; def bar[A](a: A) = (a, a); bar((A, B, C, D, E)) }
          |}
          |""".stripMargin)
    )
    test(List(code))
  }

  @Test def testSyntheticModuleLocationInDecls(): Unit = {
    def code = List(
      source("a.scala",
        """object A {
          |  class C(val a: Any) extends AnyVal
          |  implicit class I1(a: String)
          |  implicit class IV(val a: String) extends AnyVal
          |  case class CC()
          |}
          |""".stripMargin),
      source("b.scala",
        """object B {
          |  // Order here reversed from definition order in A
          |  A.CC()
          |  A.IV("")
          |  A.I1("")
          |  new A.C("")
          |}
          |""".stripMargin)
    )
    test(List(code))
  }

  // TODO: fix compiler determinism for this to pass
  @Ignore("classfile member order and InnerClasses attribute differ under separate compilation, see scala/scala3#26552")
  @Test def testReferenceToInnerClassMadeNonPrivate(): Unit = {
    def code = List(
      source("t.scala",
        """trait T {
          |  private class Inner
          |  class OtherInner { new Inner } // triggers ExpandPrivate of Inner
          |  private val v: Option[Inner] = None
          |}
          |""".stripMargin),
      source("c.scala", "class C extends T\n")
    )
    test(List(code))
  }

  @Test def testPackageObjectUserLand(): Unit = {
    def code = List(
      source("package.scala", "package userland; object `package` { type Throwy = java.lang.Throwable }\n"),
      source("th.scala", "package userland; class th[T <: Throwy](cause: T = null)\n")
    )
    test(code :: Nil)
  }

  @Ignore("TASTy differs under separate compilation (TreePickler SHAREDtype addresses), see scala/scala3#26551")
  @Test def testAnonymousGivens(): Unit = {
    def code = List(
      source("a.scala",
        """package demo
          |
          |trait Show[T]
          |object Insts {
          |  given Show[Int] = new Show[Int] {}
          |  given Show[String] = new Show[String] {}
          |}
          |""".stripMargin),
      source("b.scala",
        """package demo
          |
          |object B {
          |  val si = summon[Show[Int]](using Insts.given_Show_Int)
          |}
          |""".stripMargin)
    )
    test(List(code))
  }

  @Ignore("TASTy of synthesized Mirror differs under separate compilation, see scala/scala3#26551")
  @Test def testMirrorSynthesis(): Unit = {
    def code = List(
      source("a.scala",
        """package demo
          |
          |case class CC(x: Int, y: String)
          |""".stripMargin),
      source("b.scala",
        """package demo
          |
          |object B {
          |  val m = summon[scala.deriving.Mirror.Of[CC]]
          |}
          |""".stripMargin)
    )
    test(List(code))
  }

  @Test def testExportForwarders(): Unit = {
    def code = List(
      source("a.scala",
        """package demo
          |
          |class Impl {
          |  def f1: Int = 1
          |  def f2: Int = 2
          |  def f3: Int = 3
          |  def f4: Int = 4
          |}
          |""".stripMargin),
      source("b.scala",
        """package demo
          |
          |class Facade(impl: Impl) {
          |  export impl.*
          |}
          |""".stripMargin)
    )
    test(List(code))
  }
}

/** Harness: compile groups of sources, then recompile permutations and
 *  non-empty prefixes of the last group against the reference output, and
 *  require byte-for-byte identical `.class` and `.tasty` files.
 */
class DeterminismTester {

  case class TestSource(name: String, content: String):
    def isJava: Boolean = name.endsWith(".java")

  def source(name: String, content: String): TestSource = TestSource(name, content)

  private val compilerOptions = List(
    "-color:never",
    "-pagewidth", "120",
  )

  def test(groups: List[List[TestSource]]): Unit = {
    val srcDir = Files.createTempDirectory("determinism-src")
    val referenceOutput = Files.createTempDirectory("determinism-reference")

    def writeSources(sources: List[TestSource]): List[Path] =
      sources.map { s =>
        val p = srcDir.resolve(s.name)
        Files.write(p, s.content.getBytes(StandardCharsets.UTF_8))
        p
      }

    def compile(output: Path, sources: List[TestSource]): Unit = {
      val paths = writeSources(sources)
      val classpath = TestConfiguration.basicClasspath + java.io.File.pathSeparator + output.toString
      val (javaSources, scalaSources) = sources.partition(_.isJava)
      if scalaSources.nonEmpty then
        val args =
          compilerOptions
            ++ List("-classpath", classpath, "-d", output.toString, "-sourceroot", srcDir.toString)
            ++ paths.map(_.toString)
        val reporter = new dotty.tools.dotc.reporting.Reporter.SilentReporter
        try Main.process(args.toArray, reporter)
        catch case NonFatal(ex) =>
          throw new AssertionError(s"compiler crash while compiling ${sources.map(_.name)}", ex)
        assertFalse(
          s"compilation of ${sources.map(_.name)} failed:\n${reporter.allErrors.mkString("\n")}",
          reporter.hasErrors)
      if javaSources.nonEmpty then
        val javac = ToolProvider.getSystemJavaCompiler
        assertNotNull("No javac from getSystemJavaCompiler, JAVA_HOME must point to a JDK", javac)
        val diagnostics = new DiagnosticCollector[JavaFileObject]
        val fileMan = javac.getStandardFileManager(diagnostics, null, StandardCharsets.UTF_8)
        try
          val javaPaths = paths.zip(sources).collect { case (p, s) if s.isJava => p.toAbsolutePath.toString }
          val javaFileObjects = fileMan.getJavaFileObjects(javaPaths*)
          val options = List("-d", output.toString, "-cp", classpath)
          val task = javac.getTask(null, fileMan, diagnostics, options.asJava, null, javaFileObjects)
          val ok = task.call()
          assertTrue(s"javac failed for $javaPaths:\n${diagnostics.getDiagnostics.asScala.mkString("\n")}", ok)
        finally fileMan.close()
    }

    for group <- groups do compile(referenceOutput, group)

    val lastGroup = groups.last
    // permutationsWithSubsets is factorial in the group size and every entry
    // is a full compiler run, so fall back to a linear number of interesting
    // orderings for anything but the smallest groups.
    val permutations: List[List[TestSource]] =
      if lastGroup.size > 4 then lastGroup.reverse :: lastGroup.map(_ :: Nil)
      else permutationsWithSubsets(lastGroup)

    // Cleanup happens only on success: on failure the temp directories are
    // kept for inspection and named in the assertion message.
    for permutation <- permutations do
      val recompileOutput = Files.createTempDirectory("determinism-recompile")
      copyRecursive(referenceOutput, recompileOutput)
      compile(recompileOutput, permutation)
      assertDirectorySame(referenceOutput, recompileOutput, permutation.map(_.name).mkString("recompile of [", ", ", "]"))
      deleteDirectory(recompileOutput.toFile)

    deleteDirectory(referenceOutput.toFile)
    deleteDirectory(srcDir.toFile)
  }

  def permutationsWithSubsets[A](as: List[A]): List[List[A]] =
    as.permutations.toList.flatMap(_.inits.filter(_.nonEmpty)).distinct

  // -- Directory comparison ---------------------------------------------------

  private def isBinaryArtifact(p: Path): Boolean =
    val name = p.getFileName.toString
    name.endsWith(".class") || name.endsWith(".tasty")

  private def relativeArtifacts(dir: Path): Map[String, Path] =
    val stream = Files.walk(dir)
    try stream.iterator().asScala.filter(isBinaryArtifact).map(p => dir.relativize(p).toString -> p).toMap
    finally stream.close()

  private def assertDirectorySame(dir1: Path, dir2: Path, note: String): Unit = {
    // On failure the two directories are intentionally kept for inspection.
    def kept = s"(outputs kept for inspection: reference=$dir1, recompile=$dir2)"
    val files1 = relativeArtifacts(dir1)
    val files2 = relativeArtifacts(dir2)
    assertEquals(s"different file sets ($note)\n$kept", files1.keySet, files2.keySet)

    val diffs = files1.keySet.toList.sorted.flatMap { rel =>
      val bytes1 = Files.readAllBytes(files1(rel))
      val bytes2 = Files.readAllBytes(files2(rel))
      if java.util.Arrays.equals(bytes1, bytes2) then None
      else if rel.endsWith(".class") then Some(s"$rel differs:\n${diffClassFiles(bytes1, bytes2)}")
      else Some(s"$rel differs (TASTy):\n${diffTasty(bytes1, bytes2)}")
    }
    if diffs.nonEmpty then
      fail(s"non-deterministic output after $note:\n${diffs.mkString("\n")}\n$kept")
  }

  private def textify(bytes: Array[Byte]): List[String] =
    import scala.tools.asm.ClassReader
    import scala.tools.asm.util.TraceClassVisitor
    val sw = new StringWriter
    import dotty.tools.backend.jvm.TraceUtils.readClass
    readClass(bytes).accept(new TraceClassVisitor(new PrintWriter(sw)))
    sw.toString.linesIterator.toList

  private def diffClassFiles(bytes1: Array[Byte], bytes2: Array[Byte]): String =
    diffLines(textify(bytes1), textify(bytes2), "(byte-level difference only, e.g. constant pool order or TASTY attribute)")

  private def diffTasty(bytes1: Array[Byte], bytes2: Array[Byte]): String =
    import dotty.tools.dotc.core.tasty.TastyPrinter
    def contents(bytes: Array[Byte]): List[String] =
      // testPickler = true elides the header (version, tooling, UUID), whose
      // content-derived UUID differs whenever anything else differs.
      TastyPrinter.showContents(bytes, noColor = true, isBestEffortTasty = false, testPickler = true)
        .linesIterator.toList
    diffLines(contents(bytes1), contents(bytes2), "(difference in TASTy header only, e.g. the UUID)")

  /** Show the lines remaining after trimming the common prefix and suffix,
   *  so a single insertion does not misalign the whole comparison.
   */
  private def diffLines(lines1: List[String], lines2: List[String], fallback: String): String =
    val prefixLen = lines1.zip(lines2).takeWhile((l, r) => l == r).size
    val rest1 = lines1.drop(prefixLen)
    val rest2 = lines2.drop(prefixLen)
    val suffixLen = rest1.reverse.zip(rest2.reverse).takeWhile((l, r) => l == r).size
    val mid1 = rest1.dropRight(suffixLen)
    val mid2 = rest2.dropRight(suffixLen)
    if mid1.isEmpty && mid2.isEmpty then s"  $fallback"
    else
      def block(label: String, lines: List[String]) =
        val shown = lines.take(20).map(l => s"  $label: $l")
        if lines.sizeIs > 20 then shown :+ s"  $label: ... (${lines.size - 20} more lines)" else shown
      ((s"  (first difference at line ${prefixLen + 1})" :: block("reference", mid1)) ++ block("recompile", mid2))
        .mkString("\n")

  // -- Temp file helpers ------------------------------------------------------

  private def copyRecursive(src: Path, dest: Path): Unit =
    val stream = Files.walk(src)
    try
      stream.iterator().asScala.foreach { p =>
        val target = dest.resolve(src.relativize(p).toString)
        if Files.isDirectory(p) then Files.createDirectories(target)
        else
          Files.createDirectories(target.getParent)
          Files.copy(p, target, StandardCopyOption.REPLACE_EXISTING)
      }
    finally stream.close()
}

