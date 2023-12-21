abstract class AbstractFile {
	def name: String
	val extension: String = name.substring(4)
}

class RemoteFile(url: String) extends AbstractFile {
	val localFile: String = s"${url.##}.tmp"  // warn: usage of `localFile` before it's initialized
	def name: String = localFile
}
