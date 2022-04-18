package dotty.tools
package dotc
package config

import scala.language.unsafeNulls

import io._

/** A class for holding mappings from source directories to
 *  their output location. This functionality can be accessed
 *  only programmatically. The command line compiler uses a
 *  single output location, but tools may use this functionality
 *  to set output location per source directory.
 */
class OutputDirs {
  /** Pairs of source directory - destination directory. */
  private var outputDirs: List[(AbstractFile, AbstractFile)] = Nil

  /** If this is not None, the output location where all
   *  classes should go.
   */
  private var singleOutDir: Option[AbstractFile] = None

  /** Add a destination directory for sources found under srcdir.
   *  Both directories should exits.
   */
  def add(srcDir: String, outDir: String): Unit =
    add(checkDir(AbstractFile.getDirectory(srcDir), srcDir),
      checkDir(AbstractFile.getDirectory(outDir), outDir))

  /** Check that dir is exists and is a directory. */
  private def checkDir(dir: AbstractFile, name: String, allowJar: Boolean = false): AbstractFile = (
    if (dir != null && dir.isDirectory)
      dir
    // was:      else if (allowJar && dir == null && Path.isJarOrZip(name, false))
    else if (allowJar && dir == null && Jar.isJarOrZip(File(name), false))
      new PlainFile(Path(name))
    else
      throw new FatalError(name + " does not exist or is not a directory"))

  /** Set the single output directory. From now on, all files will
   *  be dumped in there, regardless of previous calls to 'add'.
   */
  def setSingleOutput(outDir: String): Unit = {
    val dst = AbstractFile.getDirectory(outDir)
    setSingleOutput(checkDir(dst, outDir, true))
  }

  def getSingleOutput: Option[AbstractFile] = singleOutDir

  /** Set the single output directory. From now on, all files will
   *  be dumped in there, regardless of previous calls to 'add'.
   */
  def setSingleOutput(dir: AbstractFile): Unit =
    singleOutDir = Some(dir)

  def add(src: AbstractFile, dst: AbstractFile): Unit = {
    singleOutDir = None
    outputDirs ::= ((src, dst))
  }

  /** Return the list of source-destination directory pairs. */
  def outputs: List[(AbstractFile, AbstractFile)] = outputDirs

  /** Return the output directory for the given file.
   */
  def outputDirFor(src: AbstractFile): AbstractFile = {
    def isBelow(srcDir: AbstractFile, outDir: AbstractFile) =
      src.path.startsWith(srcDir.path)

    singleOutDir match {
      case Some(d) => d
      case None =>
        (outputs find (isBelow _).tupled) match {
          case Some((_, d)) => d
          case _ =>
            throw new FatalError("Could not find an output directory for "
              + src.path + " in " + outputs)
        }
    }
  }

  /** Return the source file path(s) which correspond to the given
   *  classfile path and SourceFile attribute value, subject to the
   *  condition that source files are arranged in the filesystem
   *  according to Java package layout conventions.
   *
   *  The given classfile path must be contained in at least one of
   *  the specified output directories. If it does not then this
   *  method returns Nil.
   *
   *  Note that the source file is not required to exist, so assuming
   *  a valid classfile path this method will always return a list
   *  containing at least one element.
   *
   *  Also that if two or more source path elements target the same
   *  output directory there will be two or more candidate source file
   *  paths.
   */
  def srcFilesFor(classFile: AbstractFile, srcPath: String): List[AbstractFile] = {
    def isBelow(srcDir: AbstractFile, outDir: AbstractFile) =
      classFile.path.startsWith(outDir.path)

    singleOutDir match {
      case Some(d) =>
        d match {
          case _: VirtualDirectory | _: io.ZipArchive => Nil
          case _ => List(d.lookupPathUnchecked(srcPath, false))
        }
      case None =>
        (outputs filter (isBelow _).tupled) match {
          case Nil => Nil
          case matches => matches.map(_._1.lookupPathUnchecked(srcPath, false))
        }
    }
  }
}
