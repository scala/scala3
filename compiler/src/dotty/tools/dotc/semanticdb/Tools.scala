package dotty.tools.dotc.semanticdb

import java.nio.file.*
import scala.jdk.CollectionConverters.*

private[semanticdb] object Tools:

  /** Converts a Path to a String that is URI encoded, without forcing absolute paths. */
  def mkURIstring(path: Path): String =
    // Calling `.toUri` on a relative path will convert it to absolute. Iteration through its parts instead preserves
    // the resulting URI as relative.
    // To prevent colon `:` from being treated as a scheme separator, prepend a slash `/` to each part to trick the URI
    // parser into treating it as an absolute path, and then strip the spurious leading slash from the final result.
    val uriParts = for part <- path.asScala yield new java.net.URI(null, null, "/" + part.toString, null)
    uriParts.mkString.stripPrefix("/")
