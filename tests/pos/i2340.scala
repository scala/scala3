sealed class Entry(path: Int)

class Test {
  def test = {
    class FileEntry() extends Entry(1)
    val f = new FileEntry()
  }
}