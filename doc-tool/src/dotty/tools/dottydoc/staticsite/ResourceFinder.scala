package dotty.tools
package dottydoc
package staticsite

trait ResourceFinder {
  /** If, for some reason, the supplied default files cannot be found - this
    * exception will be thrown in `layouts`.
    */
  final case class ResourceNotFoundException(message: String) extends Exception(message)

  protected def getResource(r: String): String =
    Option(getClass.getResourceAsStream(r))
      .map(scala.io.Source.fromInputStream(_)(scala.io.Codec.UTF8))
      .map(_.mkString)
      .getOrElse(throw ResourceNotFoundException(r))
}
