//> using options -Wall
// This test checks that -Wall turns on -Wsafe-init
abstract class AbstractFile:
  def name: String
  val extension: String = name.substring(4)

class RemoteFile(url: String) extends AbstractFile:
  val localFile: String = s"${url.##}.tmp"  // warn
  def name: String = localFile
