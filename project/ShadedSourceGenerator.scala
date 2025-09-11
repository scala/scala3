import sbt._
import sbt.Keys._

/**
 * ShadedSourceGenerator - A build plugin for creating shaded versions of external dependencies
 * 
 * This generator downloads source JARs for specified dependencies (currently pprint, fansi, and sourcecode),
 * extracts them, and applies patches to:
 * 1. Add the dotty.shaded package prefix
 * 2. Rewrite imports to use _root_ to avoid conflicts
 * 3. Apply Scala 3 compatibility fixes (mostly due to enforcing null safety in scala/scala3)
 * 
 * The shaded sources are placed in the managed source directory and included in compilation.
 * This allows the Scala 3 compiler to bundle these utilities without external dependencies.
 */
object ShadedSourceGenerator {

  val task = Def.task {
    val s = streams.value
    val cacheDir = s.cacheDirectory
    val dest = (Compile / sourceManaged).value / "downloaded"
    val lm = dependencyResolution.value

    val dependencies = Seq(
      ("com.lihaoyi", "pprint_3", "0.9.3"),
      ("com.lihaoyi", "fansi_3", "0.5.1"),
      ("com.lihaoyi", "sourcecode_3", "0.4.4"),
    )

    // Create a marker file that tracks the dependencies for cache invalidation
    val markerFile = cacheDir / "shaded-sources-marker"
    val markerContent = dependencies.map { case (org, name, version) => s"$org:$name:$version:sources" }.mkString("\n")
    if (!markerFile.exists || IO.read(markerFile) != markerContent) {
      IO.write(markerFile, markerContent)
    }

    FileFunction.cached(cacheDir / "fetchShadedSources",
      FilesInfo.lastModified, FilesInfo.exists) { _ =>
      s.log.info(s"Downloading and processing shaded sources to $dest...")

      if (dest.exists) {
        IO.delete(dest)
      }
      IO.createDirectory(dest)

      for((org, name, version) <- dependencies) {
        import sbt.librarymanagement._

        val moduleId = ModuleID(org, name, version).sources()
        val retrieveDir = cacheDir / "retrieved" / s"$org-$name-$version-sources"

        s.log.info(s"Retrieving $org:$name:$version:sources...")
        val retrieved = lm.retrieve(moduleId, scalaModuleInfo = None, retrieveDir, s.log)
        val jarFiles = retrieved.fold(
          w => throw w.resolveException,
          files => files.filter(_.getName.contains("-sources.jar"))
        )

        jarFiles.foreach { jarFile =>
          s.log.info(s"Extracting ${jarFile.getName}...")
          IO.unzip(jarFile, dest)
        }
      }

      val scalaFiles = (dest ** "*.scala").get
      
      // Define patches as a map from search text to replacement text
      val patches = Map(
        "import scala" -> "import _root_.scala",
        " scala.collection." -> " _root_.scala.collection.",
        "def apply(c: Char): Trie[T]" -> "def apply(c: Char): Trie[T] | Null",
        "var head: Iterator[T] = null" -> "var head: Iterator[T] | Null = null",
        "if (head != null && head.hasNext) true" -> "if (head != null && head.nn.hasNext) true",
        "head.next()" -> "head.nn.next()",
        "abstract class Walker" -> "@scala.annotation.nowarn abstract class Walker",
        "object TPrintLowPri" -> "@scala.annotation.nowarn object TPrintLowPri",
        "x.toString match{" -> "scala.runtime.ScalaRunTime.stringOf(x) match{"
      )
      
      val patchUsageCounter = scala.collection.mutable.Map(patches.keys.map(_ -> 0).toSeq: _*)
      
      scalaFiles.foreach { file =>
        val text = IO.read(file)
        if (!file.getName.equals("CollectionName.scala")) {
          var processedText = "package dotty.shaded\n" + text
          
          // Apply patches and count usage
          patches.foreach { case (search, replacement) =>
            if (processedText.contains(search)) {
              processedText = processedText.replace(search, replacement)
              patchUsageCounter(search) += 1
            }
          }

          IO.write(file, processedText)
        }
      }
      
      // Assert that all patches were applied at least once
      val unappliedPatches = patchUsageCounter.filter(_._2 == 0).keys
      if (unappliedPatches.nonEmpty) {
        throw new RuntimeException(s"Patches were not applied: ${unappliedPatches.mkString(", ")}")
      }

      scalaFiles.toSet
    } (Set(markerFile)).toSeq

  }

}