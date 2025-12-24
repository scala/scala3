package scala.quoted.compiletime

trait SourceFile private[compiletime] () {

  /** Path to this source file. May be `None` for virtual files such as in the REPL. */
  def getJPath: Option[java.nio.file.Path]

  /** Name of the source file. */
  def name: String

  /**
    * Path of the source file.
    *
    *  It does not necessarily point to a path in the filesystem, it could be the path of a virtual file.
    *  Use `getJPath` to get paths to the filesystem.
    */
  def path: String

  /** Content of this source file. */
  def content: Option[String]

}
object SourceFile {

  def quoted(using quotes: Quotes): SourceFile.Module = quotes.reflectV2.SourceFile
  given moduleConversion: (quotes: Quotes) => Conversion[SourceFile.type, SourceFile.Module] = _ => quotes.reflectV2.SourceFile

  trait Module private[compiletime] () {

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def current: SourceFile

  }

}
