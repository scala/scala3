package scala.quoted.meta

trait SourceFile private[meta] {

  /** Path to this source file. May be `None` for virtual files such as in the REPL. */
  def getJPath: Option[java.nio.file.Path]

  /** Name of the source file */
  def name: String

  /**
    * Path of the source file.
    *
    *  It does not necessarily point to a path in the filesystem, it could be the path of a virtual file.
    *  Use `getJPath` to get paths to the filesystem.
    */
  def path: String

  /** Content of this source file */
  def content: Option[String]

}
object SourceFile {

  def api(using meta: Meta): Meta.SourceFileAPI = meta.internal.sourceFile
  given Meta => Conversion[SourceFile.type, Meta.SourceFileAPI] = _.api

}
