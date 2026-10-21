package xsbt

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import org.junit.Test
import org.junit.Assert.*

import xsbti.api.Def
import xsbt.api.HashAPI

class ExtraTypeParametersSpecification:
  import ExtraTypeParametersSpecification.*

  @Test
  def stableAcrossJVMs(): Unit =
    assertEquals(forkedHash(0, 1), forkedHash(1000, 2))

  @Test
  def changesToBounds(): Unit =
    val original = hash("extension (receiver: Int) def choose[A](value: A): A = value")
    val upper = hash("extension (receiver: Int) def choose[A <: CharSequence](value: A): A = value")
    val lower = hash("extension (receiver: Int) def choose[A >: String](value: A): A = value")
    assertNotEquals(original, upper)
    assertNotEquals(original, lower)

  @Test
  def changesToHigherKindedBounds(): Unit =
    val original = hash("def choose(receiver: Int)[F[X]](value: Int): Int = value")
    val changed = hash("def choose(receiver: Int)[F[X <: CharSequence]](value: Int): Int = value")
    assertNotEquals(original, changed)

  @Test
  def positionsOfTypeParameterLists(): Unit =
    val original = hash("def choose(x: Int)[A](y: Int)(z: Int): Int = z")
    val changed = hash("def choose(x: Int)(y: Int)[A](z: Int): Int = z")
    assertNotEquals(original, changed)

  private def forkedHash(allocations: Int, unrelated: Int): Int =
    val output = Files.createTempFile("extra-type-parameters", ".log")
    val java = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val process = new ProcessBuilder(
      java, "-cp", System.getProperty("java.class.path"),
      "xsbt.ExtraTypeParametersSpecification", allocations.toString, unrelated.toString
    ).redirectErrorStream(true).redirectOutput(output.toFile).start()
    try
      assertTrue("compiler JVM timed out", process.waitFor(60, TimeUnit.SECONDS))
      val log = Files.readString(output)
      assertEquals(log, 0, process.exitValue())
      log.linesIterator.find(_.startsWith("API_HASH=")).get.stripPrefix("API_HASH=").toInt
    finally
      process.destroyForcibly()
      Files.deleteIfExists(output)

object ExtraTypeParametersSpecification:
  def hash(method: String, unrelated: Int = 1): Int =
    val source = s"""package repro
                   |def unrelated: Int = $unrelated
                   |$method
                   |""".stripMargin
    val apis = new ScalaCompilerForUnitTesting().extractApiFromSrc(source)
    val definition = apis.iterator.flatMap(_.structure().declared()).collectFirst:
      case method: Def if method.name() == "choose" => method
    HashAPI(definition.get)

  def main(args: Array[String]): Unit =
    // Perturb identity hash allocation before the API enums are initialized.
    for _ <- 0 until args(0).toInt do
      System.identityHashCode(new Object)
    println(s"API_HASH=${hash("extension (receiver: Int) def choose[A](value: A): A = value", args(1).toInt)}")
