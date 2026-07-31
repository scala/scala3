package dotty.tools.nio

import java.util.Locale

/**
 * Represents a file extension, such as "scala" or "tasty".
 * Case-insensitive.
 * The file extension is always the last thing after a period in a file name,
 * e.g., "file.test.scala" has the extension "scala".
 */
class FileExtension private(value: String) extends AnyVal {
  /** The full extension including a leading dot if not empty. */
  def withDot: String =
    if value == "" then ""
    else "." + value

  /** Whether this is the `".betasty"` extension. */
  def isBetasty: Boolean = value == "betasty"
  /** Whether this is the  `".class"` extension. */
  def isClass: Boolean = value == "class"
  /** Whether this is the  `".java"` extension. */
  def isJava: Boolean = value == "java"
  /** Whether this is the  `".jar"` extension. */
  def isJar: Boolean = value == "jar"
  /** Whether this is the  `".scala"` extension. */
  def isScala: Boolean = value == "scala"
  /** Whether this is the  `".tasty"` extension. */
  def isTasty: Boolean = value == "tasty"
  /** Whether this is the  `".zip"` extension. */
  def isZip: Boolean = value == "zip"

  /** Whether this is the given extension. */
  def is(other: String): Boolean = value == other.toLowerCase(Locale.ROOT)
}

object FileExtension {
  given string2ext: Conversion[String, FileExtension] with
    def apply(s: String): FileExtension = FileExtension(s)

  def apply(value: String) =
    new FileExtension(value.toLowerCase(Locale.ROOT))
}
