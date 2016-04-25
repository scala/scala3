package dotty.tools
package dottydoc

import dotc.core.Contexts.Context
import dotc.config.{ScalaSettings, Settings}
import Settings._

class DottyDocSettings extends ScalaSettings {
  /** A setting that defines the overall title of the documentation, typically the name of the library being
    * documented. */
  val doctitle = StringSetting (
    "-doc-title",
    "title",
    "The overall name of the Scaladoc site",
    ""
  )

  /** A setting that defines the overall version number of the documentation, typically the version of the library being
    * documented. */
  val docversion = StringSetting (
    "-doc-version",
    "version",
    "An optional version number, to be appended to the title",
    ""
  )

  val docfooter = StringSetting (
    "-doc-footer",
    "footer",
    "A footer on every Scaladoc page, by default the EPFL/Lightbend copyright notice. Can be overridden with a custom footer.",
    ""
  )

  val docUncompilable = StringSetting (
    "-doc-no-compile",
    "path",
    "A directory containing sources which should be parsed, no more (e.g. AnyRef.scala)",
    ""
  )

  def uncompilableFiles(implicit ctx: Context) = docUncompilable.value match {
    case ""     => Nil
    case path   => io.Directory(path).deepFiles.filter(_ hasExtension "scala").toList
  }

  val docExternalDoc = MultiStringSetting (
    "-doc-external-doc",
    "external-doc",
    "comma-separated list of classpath_entry_path#doc_URL pairs describing external dependencies."
  )

  val docAuthor = BooleanSetting("-author", "Include authors.", true)

  val docGroups = BooleanSetting (
    "-groups",
    "Group similar functions together (based on the @group annotation)"
  )
}
