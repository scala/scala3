package dotty.tools.dotc.classpath

import scala.language.unsafeNulls

import dotty.tools.dotc.core.Contexts.Context

import java.io.{ByteArrayOutputStream, IOException}
import java.nio.file.{FileSystems, Files, Path}
import java.util.jar.Attributes
import java.util.jar.Attributes.Name

import org.junit.Assert._
import org.junit.Test

import scala.jdk.CollectionConverters._
import scala.util.Properties

class MultiReleaseJarTest extends dotty.tools.backend.jvm.DottyBytecodeTest {

  @Test
  def mrJar(): Unit = {
    if (!Properties.isJavaAtLeast("9")) { println("skipping mrJar() on old JDK"); return }

    // The test fails if the same jar file gets reused. This might be a caching problem in our classpath implementation

    val jar1 = Files.createTempFile("mr-jar-test-", ".jar")
    val jar3 = Files.createTempFile("mr-jar-test-", ".jar")
    val jar2 = Files.createTempFile("mr-jar-test-", ".jar")

    def classBytes(code: String): Array[Byte] =
      val outDir = compileCode(code :: Nil)
      getGeneratedClassfiles(outDir).head._2

    val defaultFooDef = "package p1; abstract class Foo { def foo1: Int }"
    val defaultBarDef = "package p2; abstract class Bar { def bar1: Int }"
    val java9FooDef = "package p1; abstract class Foo { def foo1: Int; def foo2: Int }"
    val java10BarDef = "package p2; abstract class Bar { def bar1: Int; def bar2: Int }"

    def apiMethods(jarPath: Path, release: String): Set[String] = {
      given ctx: Context = initCtx.fresh
      ctx.settings.usejavacp.update(true)
      ctx.settings.classpath.update(jarPath.toAbsolutePath.toString)
      ctx.settings.javaOutputVersion.update(release)
      ctx.initialize()
      val classNames = Seq("p1.Foo",  "p2.Bar")
      val classFiles = classNames.flatMap(ctx.platform.classPath.findClassFile)
      val classNodes = classFiles.map(classFile => loadClassNode(classFile.input))
      val methodNames = classNodes.flatMap(_.methods.asScala).map(_.name)
      methodNames.filter(_ != "<init>").toSet
    }

    try {
      List(jar1, jar2, jar3).foreach(temp => createZip(temp, List(
        "/p1/Foo.class" -> classBytes(defaultFooDef),
        "/p2/Bar.class" -> classBytes(defaultBarDef),
        "/META-INF/versions/9/p1/Foo.class" -> classBytes(java9FooDef),
        "/META-INF/versions/10/p2/Bar.class" -> classBytes(java10BarDef),
        "/META-INF/MANIFEST.MF" -> createManifest)
      ))

      assertEquals(Set("foo1", "bar1"), apiMethods(jar1, "8"))
      assertEquals(Set("foo1", "foo2", "bar1"), apiMethods(jar2, "9"))

      if Properties.isJavaAtLeast("10") then
        assertEquals(Set("foo1", "foo2", "bar1", "bar2"), apiMethods(jar3, "10"))
    } finally
      List(jar1, jar2, jar3).forall(path =>
        try Files.deleteIfExists(path)
        catch case _: IOException => false
      )
  }

  @Test
  def ctSymTest(): Unit = {
    if (!Properties.isJavaAtLeast("9")) { println("skipping mrJar() on old JDK"); return }

    def classExists(className: String, release: String): Boolean = {
      given ctx: Context = initCtx.fresh
      ctx.settings.usejavacp.update(true)
      ctx.settings.javaOutputVersion.update(release)
      ctx.initialize()
      val classFile = ctx.platform.classPath.findClassFile(className)
      classFile.isDefined
    }

    assertFalse(classExists("java.lang.invoke.LambdaMetafactory", "7"))
    assertTrue(classExists("java.lang.invoke.LambdaMetafactory", "8"))
    assertTrue(classExists("java.lang.invoke.LambdaMetafactory", "9"))
  }

  private def createManifest = {
    val manifest = new java.util.jar.Manifest()
    manifest.getMainAttributes.put(Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(new Attributes.Name("Multi-Release"), String.valueOf(true))
    val os = new ByteArrayOutputStream()
    manifest.write(os)
    val manifestBytes = os.toByteArray
    manifestBytes
  }

  private def createZip(zipLocation: Path, content: List[(String, Array[Byte])]): Unit = {
    val env = new java.util.HashMap[String, String]()
    Files.deleteIfExists(zipLocation)
    env.put("create", String.valueOf(true))
    val fileUri = zipLocation.toUri
    val zipUri = new java.net.URI("jar:" + fileUri.getScheme, fileUri.getPath, null)
    val zipfs = FileSystems.newFileSystem(zipUri, env)
    try {
      try {
        for ((internalPath, contentBytes) <- content) {
          val internalTargetPath = zipfs.getPath(internalPath)
          Files.createDirectories(internalTargetPath.getParent)
          Files.write(internalTargetPath, contentBytes)
        }
      } finally {
        if (zipfs != null) zipfs.close()
      }
    } finally {
      zipfs.close()
    }
  }

}
