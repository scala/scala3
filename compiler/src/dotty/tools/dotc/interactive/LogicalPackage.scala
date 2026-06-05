package dotty.tools.dotc.interactive

import dotty.tools.io.AbstractFile

/**
 * A logical package representation. This is disconnected from the file system, and faithfully
 * represents the nesting of packages and sources that contribute classes to those packages.
 */
trait LogicalPackage {

  def name: String

  def packages: Seq[LogicalPackage]

  /**
   * Return all sources contained by this package. Only direct members are returned, and there are no duplicates.
   *
   */
  def sources: Seq[AbstractFile]

  def getPackage(name: String): Option[LogicalPackage]

  def prettyPrint(): String = {
    prettyPrintWith().toString().stripTrailing().stripIndent()
  }

  private def prettyPrintWith(
      indent: Int = 0,
      sb: StringBuilder = new StringBuilder
  ): StringBuilder =
    sb ++= " " * indent
    sb ++= s"$name\n"
    packages.sortBy(_.name).foreach(_.prettyPrintWith(indent + 4, sb))
    sources.foreach { s =>
      sb ++= (" " * (indent + 4))
      sb ++= s.name + "\n"
    }
    sb

}
