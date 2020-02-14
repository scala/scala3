abstract class AbstractFile {
	def name: String
	val extension: String = name.substring(4)
}

class RemoteFile(url: String) extends AbstractFile {
	val localFile: String = url.hashCode + ".tmp"  // error
	def name: String = localFile
}
