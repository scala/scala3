package dotty
package tools
package dotc

import scala.jdk.CollectionConverters.*

import dotty.tools.nio.*
import vulpix.*

object IdempotencyCheck {
  def checkIdempotency(first: String, second: String): Unit = {
    var failed = 0
    var total = 0
    val dir1 = FileContainer.getOnDisk(first).get
    val dir2 = FileContainer.getOnDisk(second).get

    val groupedBytecodeFiles: Iterable[(File, File, File, File)] = {
      val bytecodeFiles = {
        def bytecodeFiles(paths: Iterable[FileSystemEntry], dir: String): Iterable[(String, File)] =
          paths.collect{ case f: File if f.extension.isClass || f.extension.isTasty => (f.nameWithoutExtension, f) }
        bytecodeFiles(dir1.recursiveEntries, dir1.path) ++ bytecodeFiles(dir2.recursiveEntries, dir2.path)
      }
      val groups = bytecodeFiles.groupBy(_._1).view.mapValues(_.map(_._2))

      groups.iterator.flatMap { g =>
        def pred(f: File, dir: String, isTasty: Boolean) =
          f.path.contains(dir) && (if isTasty then f.extension.isTasty else f.extension.isClass)
        val class1 = g._2.find(f => pred(f, dir1.path, isTasty = false))
        val class2 = g._2.find(f => pred(f, dir2.path, isTasty = false))
        val tasty1 = g._2.find(f => pred(f, dir1.path, isTasty = true))
        val tasty2 = g._2.find(f => pred(f, dir2.path, isTasty = true))
        assert(class1.isDefined, s"Could not find class in ${dir1.path} for ${g._1}")
        assert(class2.isDefined, s"Could not find class in ${dir2.path} for ${g._1}")
        if (tasty1.isEmpty || tasty2.isEmpty) Nil
        else List(Tuple4(class1.get, tasty1.get, class2.get, tasty2.get))
      }.toList
    }

    for ((class1, tasty1, class2, tasty2) <- groupedBytecodeFiles) {
      total += 1
      val bytes1 = class1.readBytes()
      val bytes2 = class2.readBytes()
      if (!java.util.Arrays.equals(bytes1, bytes2)) {
        failed += 1
        val tastyBytes1 = tasty1.readBytes()
        val tastyBytes2 = tasty2.readBytes()
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

    assert(failed == 0, s"Failed $failed idempotency checks (out of $total outputs)")
    println(s"== Checked $total outputs for idempotency ==")
  }
}
