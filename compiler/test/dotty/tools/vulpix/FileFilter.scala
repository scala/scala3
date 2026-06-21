package dotty.tools.vulpix

sealed trait FileFilter {
  def accept(file: String): Boolean
}

object FileFilter {
  def exclude(file: String): FileFilter = exclude(file +: Vector())

  def exclude(file: String, files: String*): FileFilter =
    exclude(file +: files.toVector)

  def exclude(files: Vector[String]): FileFilter = new FileFilter {
    private val excluded = files.toSet
    def accept(file: String): Boolean = !excluded.contains(file)
  }

  def include(files: Vector[String]): FileFilter = new FileFilter {
    private val included = files.toSet
    def accept(file: String): Boolean = included.contains(file)
  }

  object NoFilter extends FileFilter {
    def accept(file: String) = true
  }
}
