package scala3build

import mill.*

/**
 * Tasks shared by the main standard library, and the Scala.js standard library
 */
trait SharedLibrarySettings extends CrossScala3Module {

  /** Whether we are compiling the Scala.js standard library or not */
  def isJS: Boolean = false

  def scalacOptions = super.scalacOptions() ++ Seq(
    "-Yexplicit-nulls",
    "-Wsafe-init"
  )

  /** JARs or directories with classes (and sjsir if `isJS` is true) to be stripped of their Scala 2 annotations and copied in the class directory of this module */
  def patches: T[Seq[PathRef]]

  def compile = Task(persistent = true) {

    // Add classes (and sjsir files if isJS is true) from `patches` to the class directory of this module

    // Important: given this is a persistent task, if you change anything in the stuff being done here,
    // should should manually wipe out the compile directories to clean the cache and make this to be
    // re-computed.

    val res = super.compile()
    val patches0 = patches()
    val sigFile = Task.dest / "sig"
    val updatedClassDir = Task.dest / "classes"

    val signature = s"${res.classes.sig}-${patches0.map(_.sig).mkString("-")}"
    val needsUpdate = !res.classes.validate() ||
      patches0.exists(!_.validate()) ||
      !os.exists(sigFile) ||
      os.read(sigFile) != signature

    if (needsUpdate) {
      os.remove.all(updatedClassDir)
      os.makeDir.all(updatedClassDir)
      for (elem <- os.list(res.classes.path))
        os.copy(elem, updatedClassDir / elem.last)

      val keepExtensions = if (isJS) Seq(".class", ".sjsir") else Seq(".class")

      def process(relPath: os.SubPath, dir: os.Path): Unit = {
        val shortName = keepExtensions.foldLeft(relPath.toString)(_.stripSuffix(_))
        val dest = updatedClassDir / relPath
        val shouldCopy = ScalaLibraryFilesToCopy.filesToCopy.contains(shortName) || ScalaLibraryFilesToCopy.filesToCopy.exists(n => shortName.startsWith(n + "$"))

        if (shouldCopy)
          System.err.println(s"Overwriting $relPath")
        else if (!os.exists(dest))
          System.err.println(s"Copying $relPath")

        if (shouldCopy || !os.exists(dest))
          StripScala2Annotations.patchFile((dir / relPath).toIO, (updatedClassDir / relPath).toIO, updatedClassDir.toIO)
      }

      for ((jarOrDir, jarOrDirIdx) <- patches0.map(_.path).zipWithIndex) {
        System.err.println(s"Copying entries from $jarOrDir")

        val finalDir =
          if (os.isDir(jarOrDir)) jarOrDir
          else
            // Extracting things on disk rather than reading directly from the zip,
            // in order to pass things to the current StripScala2Annotations methods,
            // that we'd rather not change for now, given they belong to the sbt build
            os.unzip(jarOrDir, Task.dest / "tmp" / s"$jarOrDirIdx")

        val files = os.walk(finalDir)
          .filter(f => keepExtensions.exists(f.last.endsWith))
          .filter(os.isFile)
          .map(_.subRelativeTo(finalDir))
          .sorted

        for (f <- files)
          process(f, finalDir)
      }

      os.write.over(sigFile, signature)
    }

    mill.javalib.api.CompilationResult(res.analysisFile, PathRef(updatedClassDir))
  }

  def jar = Task {
    val jarRef = super.jar()
    ScalaJarValidate.validateNoScala2Pickles(jarRef.path.toIO)
    ScalaJarValidate.validateTastyAttributes(jarRef.path.toIO)
    ScalaJarValidate.validateScalaAttributes(jarRef.path.toIO)
    jarRef
  }
}
