object Shading {
  /** Replace package names in package definitions, for shading.
   * It assumes the full package def is written on a single line.
   * It does not adapt the imports accordingly.
   */
  def replacePackage(lines: List[String])(replace: PartialFunction[String, String]): List[String] = {
    def recur(lines: List[String]): List[String] =
      lines match {
        case head :: tail =>
          if (head.startsWith("package ")) {
            val packageName = head.stripPrefix("package ").trim
            val newPackageName = replace.applyOrElse(packageName, (_: String) => packageName)
            s"package $newPackageName" :: tail
          } else head :: recur(tail)
        case _ => lines
      }
    recur(lines)
  }

  /** Insert UnsafeNulls Import after package */
  def insertUnsafeNullsImport(lines: List[String]): List[String] = {
    def recur(ls: List[String], foundPackage: Boolean): List[String] = ls match {
      case l :: rest =>
        val lt = l.trim()
        if (foundPackage) {
          if (!(lt.isEmpty || lt.startsWith("package ")))
            "import scala.language.unsafeNulls" :: ls
          else l :: recur(rest, foundPackage)
        } else {
          if (lt.startsWith("package ")) l +: recur(rest, true)
          else l :: recur(rest, foundPackage)
        }
      case _ => ls
    }
    recur(lines, false)
  }

  /** replace imports of `com.google.protobuf.*` with compiler implemented version */
  def replaceProtobuf(lines: List[String]): List[String] = {
    def recur(ls: List[String]): List[String] = ls match {
      case l :: rest =>
        val lt = l.trim()
        if (lt.isEmpty || lt.startsWith("package ") || lt.startsWith("import ")) {
          val newLine =
            if (lt.startsWith("import com.google.protobuf.")) {
              if (lt == "import com.google.protobuf.CodedInputStream") {
                "import dotty.tools.dotc.semanticdb.internal.SemanticdbInputStream as CodedInputStream"
              } else if (lt == "import com.google.protobuf.CodedOutputStream") {
                "import dotty.tools.dotc.semanticdb.internal.SemanticdbOutputStream as CodedOutputStream"
              } else {
                l
              }
            } else {
              l
            }
          newLine :: recur(rest)
        } else {
          ls // don't check rest of file
        }
      case _ => ls
    }
    recur(lines)
  }
}
