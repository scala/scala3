//> using options -Yexplicit-nulls

import scala.language.unsafeNulls

object Lookup:

  abstract class AbstractFile2:
    def lookupName(name: String, directory: Boolean): AbstractFile2 | Null

  private final def lookupPath2(base: AbstractFile2, pathParts: Seq[String]): AbstractFile2 | Null =
    var file: AbstractFile2 | Null = base
    file.lookupName(pathParts.last, true)
