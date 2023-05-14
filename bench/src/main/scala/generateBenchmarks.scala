package dotty.tools.benchmarks

import java.nio.file.{Files, Paths, Path}
import java.util.Random

/** Generates benchmarks in `genDirName`.
 *
 *  Called automatically by the benchmarks runner ([[Bench.main]]).
 */
def generateBenchmarks(genDirName: String) =
  val thisFile = Paths.get("src/main/scala/generateBenchmarks.scala")
  val genDir = Paths.get(genDirName)

  def generateBenchmark(subDirName: String, fileName: String, make: () => String) =
    val outputDir = genDir.resolve(Paths.get(subDirName))
    Files.createDirectories(outputDir)
    val file = outputDir.resolve(Paths.get(fileName))
    if !Files.exists(file) ||
        Files.getLastModifiedTime(file).toMillis() <
        Files.getLastModifiedTime(thisFile).toMillis() then
      println(f"Generate benchmark $file")
      Files.write(file, make().getBytes())

  // Big compile-time sums of constant integer types: (1.type + 2.type + …).
  // This should ideally have a linear complexity.
  generateBenchmark("compiletime-ops", "sum-constants.scala", () =>
    val innerSum = (1 to 50) // Limited to 50 to avoid stackoverflows in the compiler.
      .map(i => f"$i")
      .mkString(" + ")
    val outerSum = (1 to 50)
      .map(_ => f"($innerSum)")
      .mkString(" + ")
    val vals = (1 to 50)
      .map(i => f"val v$i: $outerSum = ???")
      .mkString("\n\n  ")

    f"""
import scala.compiletime.ops.int.*

object Test:
  val one: 1 = ???
  val n: Int = ???
  val m: Int = ???

  $vals
    """
  )

  // Big compile-time sums of term reference types: (one.type + m.type + n.type
  // + one.type + m.type + n.type + …). This big type is normalized to (8000 +
  // 8000 * m.type + 8000 * n.type).
  generateBenchmark("compiletime-ops", "sum-termrefs.scala", () =>
    val innerSum = (1 to 40)
      .map(_ => "one.type + m.type + n.type")
      .mkString(" + ")
    val outerSum = (1 to 20)
      .map(_ => f"($innerSum)")
      .mkString(" + ")
    val vals = (1 to 4)
      .map(i => f"val v$i: $outerSum = ???")
      .mkString("\n\n  ")

    f"""
import scala.compiletime.ops.int.*

object Test:
  val one: 1 = ???
  val n: Int = ???
  val m: Int = ???

  $vals
    """
  )

  // Big compile-time sums of term references: (n + m + …). The result type is
  // inferred. The goal of this benchmark is to measure the performance cost of
  // inferring precise types for arithmetic operations.
  generateBenchmark("compiletime-ops", "sum-termrefs-terms.scala", () =>
    val innerSum = (1 to 40)
      .map(_ => "one + m + n")
      .mkString(" + ")
    val outerSum = (1 to 20)
      .map(_ => f"($innerSum)")
      .mkString(" + ")
    val vals = (1 to 4)
      .map(i => f"val v$i = $outerSum")
      .mkString("\n\n  ")

    f"""
import scala.compiletime.ops.int.*

object Test:
  val one: 1 = ???
  val n: Int = ???
  val m: Int = ???

  $vals
    """
  )

  // Big compile-time product of sums of term references: (one + n + m) * (one +
  // n + m) * …. The goal of this benchmark is to measure the performance impact
  // of distributing addition over multiplication during compile-time operations
  // normalization.
  generateBenchmark("compiletime-ops", "distribute.scala", () =>
    val product = (1 to 18)
      .map(_ => "(one.type + m.type + n.type)")
      .mkString(" * ")
    val vals = (1 to 50)
      .map(i => f"val v$i: $product = ???")
      .mkString("\n\n  ")

    f"""
import scala.compiletime.ops.int.*

object Test:
  val one: 1 = ???
  val n: Int = ???
  val m: Int = ???

  $vals
    """
  )

  def applicationCount = 14
  def applicationDepth = 10
  def applicationVals = 2

  // Compile-time sums of big applications: Op[Op[…], Op[…]] + Op[Op[…], Op[…]]
  // + …. Applications are deep balanced binary trees only differing in their
  // very last (top-right) leafs. These applications are compared pairwise in
  // order to sort the terms of the sum.
  generateBenchmark("compiletime-ops", "sum-applications.scala", () =>
    def makeOp(depth: Int, last: Boolean, k: Int): String =
      if depth == 0 then f"Op[one.type, ${if last then k.toString else "n.type"}]"
      else f"Op[${makeOp(depth - 1, false, k)}, ${makeOp(depth - 1, last, k)}]"
    val sum = (applicationCount to 1 by -1)
      .map(k => makeOp(applicationDepth, true, k))
      .mkString(" + ")
    val vals = (1 to applicationVals)
      .map(i => f"val v$i: $sum = ???")
      .mkString("\n\n  ")

    f"""
import scala.compiletime.ops.int.*

object Test:
  val one: 1 = ???
  val n: Int = ???
  type SInt = Int & Singleton
  type Op[A <: SInt, B <: SInt] <:SInt

  $vals
    """
  )
