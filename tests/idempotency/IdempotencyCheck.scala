
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

  def checkIdempotency(dir1: String, dir2: String): Unit = {
    var failed = 0
    var total = 0

    val groupedBytecodeFiles: List[(JPath, JPath, JPath, JPath)] = {
      val bytecodeFiles = {
        def bytecodeFiles(paths: JStream[JPath], dir: String): List[(String, JPath)] = {
          def isBytecode(file: String) = file.endsWith(".class") || file.endsWith(".tasty")
          def tupleWithName(f: JPath) = (f.toString.substring(dir.length + 1, f.toString.length - 6), f)
          paths.iterator.asScala.filter(path => isBytecode(path.toString)).map(tupleWithName).toList
        }
        val compilerDir1 = JPaths.get(dir1)
        val compilerDir2 = JPaths.get(dir2)
        bytecodeFiles(JFiles.walk(compilerDir1), dir1) ++ bytecodeFiles(JFiles.walk(compilerDir2), dir2)
      }
      val groups = bytecodeFiles.groupBy(_._1).mapValues(_.map(_._2))

      groups.filterNot(x => blacklisted(x._1)).valuesIterator.flatMap { g =>
        def pred(f: JPath, dir: String, isTasty: Boolean) =
          f.toString.contains(dir) && f.toString.endsWith(if (isTasty) ".tasty" else ".class")
        val class1 = g.find(f => pred(f, dir1, isTasty = false))
        val class2 = g.find(f => pred(f, dir2, isTasty = false))
        val tasty1 = g.find(f => pred(f, dir1, isTasty = true))
        val tasty2 = g.find(f => pred(f, dir2, isTasty = true))
        assert(class1.isDefined, s"Could not find class in ${dir1} for $class2")
        assert(class2.isDefined, s"Could not find class in ${dir2} for $class1")
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
