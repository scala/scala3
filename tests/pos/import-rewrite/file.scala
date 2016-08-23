package file

class File private (val str: String) {
  def name: String = "name"
}

object File {
  def apply(str: String): File = new File(str)
}

