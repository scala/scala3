
import java.io.File as JFile
import java.nio.file.{ Files as JFiles, Path as JPath, Paths as JPaths }
import java.util.stream.Stream as JStream

import scala.jdk.CollectionConverters.*

object IdempotencyCheck {
  def checkIdempotency(dir1: String, dir2: String): Unit = {
    var failed = 0
    var total = 0
    val dir1Path = JPaths.get(dir1)
    val dir2Path = JPaths.get(dir2)
    val dir1String = dir1Path.toString
    val dir2String= dir2Path.toString

    val groupedBytecodeFiles: List[(JPath, JPath, JPath, JPath)] = {
      val bytecodeFiles = {
        def bytecodeFiles(paths: JStream[JPath], dir: String): List[(String, JPath)] = {
          def isBytecode(file: String) = file.endsWith(".class") || file.endsWith(".tasty")
          def tupleWithName(f: JPath) = (f.toString.substring(dir.length, f.toString.length - 6), f)
          paths.iterator.asScala.filter(path => isBytecode(path.toString)).map(tupleWithName).toList
        }
        bytecodeFiles(JFiles.walk(dir1Path), dir1String) ++ bytecodeFiles(JFiles.walk(dir2Path), dir2String)
      }
      val groups = bytecodeFiles.groupBy(_._1).mapValues(_.map(_._2))

      groups.iterator.flatMap { g =>
        def pred(f: JPath, dir: String, isTasty: Boolean) =
          f.toString.contains(dir) && f.toString.endsWith(if (isTasty) ".tasty" else ".class")
        val class1 = g._2.find(f => pred(f, dir1String, isTasty = false))
        val class2 = g._2.find(f => pred(f, dir2String, isTasty = false))
        val tasty1 = g._2.find(f => pred(f, dir1String, isTasty = true))
        val tasty2 = g._2.find(f => pred(f, dir2String, isTasty = true))
        assert(class1.isDefined, s"Could not find class in ${dir1} for ${g._1}")
        assert(class2.isDefined, s"Could not find class in ${dir2} for ${g._1}")
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
          println(s"Idempotency test failed between $class1 and $class2 (same tasty)")
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
