package dotty.tools.vulpix

sealed trait FileFilter {
  def accept(file: String): Boolean
}

object FileFilter {

  def exclude(files: List[String]): FileFilter = new FileFilter {
    private val blackList = files.toSet
    def accept(file: String): Boolean = !blackList.contains(file)
  }

  def include(files: List[String]): FileFilter = new FileFilter {
    private val whiteList = files.toSet
    def accept(file: String): Boolean = whiteList.contains(file)
  }

  object NoFilter extends FileFilter {
    def accept(file: String) = true
  }
}
