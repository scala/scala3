
import java.nio.file.{ Files => JFiles, Path => JPath, Paths => JPaths }
import java.util.stream.{ Stream => JStream }

import scala.collection.JavaConverters._

object IdempotencyCheck {
  val blacklisted = Set(
    // No fix needed. Bridges on collections in different order. Second one in scala2 order.
    "pos/Map/scala/collection/immutable/Map",
    "pos/Map/scala/collection/immutable/AbstractMap",
    "pos/t1203a/NodeSeq",
    "pos/i2345/Whatever"
  )

  def checkIdempotency(dirPrefix: String): Unit = {
    var failed = 0
    var total = 0

    val groupedBytecodeFiles: List[(JPath, JPath, JPath, JPath)] = {
      val bytecodeFiles = {
        def bytecodeFiles(paths: JStream[JPath]): List[JPath] = {
          def isBytecode(file: String) = file.endsWith(".class") || file.endsWith(".tasty")
          paths.iterator.asScala.filter(path => isBytecode(path.toString)).toList
        }
        val compilerDir1 = JPaths.get(dirPrefix + 1)
        val compilerDir2 = JPaths.get(dirPrefix + 2)
        bytecodeFiles(JFiles.walk(compilerDir1)) ++ bytecodeFiles(JFiles.walk(compilerDir2))
      }
      val groups = bytecodeFiles.groupBy(f => f.toString.substring(dirPrefix.length + 1, f.toString.length - 6))

      groups.filterNot(x => blacklisted(x._1)).valuesIterator.flatMap { g =>
        def pred(f: JPath, i: Int, isTasty: Boolean) =
          f.toString.contains(dirPrefix + i) && f.toString.endsWith(if (isTasty) ".tasty" else ".class")
        val class1 = g.find(f => pred(f, 1, isTasty = false))
        val class2 = g.find(f => pred(f, 2, isTasty = false))
        val tasty1 = g.find(f => pred(f, 1, isTasty = true))
        val tasty2 = g.find(f => pred(f, 2, isTasty = true))
        assert(class1.isDefined, s"Could not find class in ${dirPrefix + 1} for $class2")
        assert(class2.isDefined, s"Could not find class in ${dirPrefix + 2} for $class1")
        if (tasty1.isEmpty || tasty2.isEmpty) Nil
        else List(Tuple4(class1.get, tasty1.get, class2.get, tasty2.get))
      }.toList
    }

    for ((class1, tasty1, class2, tasty2) <- groupedBytecodeFiles) {
      total += 1
      val bytes1 = JFiles.readAllBytes(class1)
      val bytes2 = JFiles.readAllBytes(class2)
      if (!java.util.Arrays.equals(bytes1, bytes2)) {
        failed += 1
        val tastyBytes1 = JFiles.readAllBytes(tasty1)
        val tastyBytes2 = JFiles.readAllBytes(tasty2)
        if (java.util.Arrays.equals(tastyBytes1, tastyBytes2))
          println(s"Idempotency test failed between $class1 and $class1 (same tasty)")
        else
          println(s"Idempotency test failed between $tasty1 and $tasty2")
        /* Dump bytes to console, could be useful if issue only appears in CI.
         * Create the .class locally with JFiles.write(path, Array[Byte](...)) with the printed array
         */
        // println(bytes1.mkString("Array[Byte](", ",", ")"))
        // println(bytes2.mkString("Array[Byte](", ",", ")"))
      }
    }

    assert(failed == 0, s"Failed $failed idempotency checks (out of $total)")
  }
}
