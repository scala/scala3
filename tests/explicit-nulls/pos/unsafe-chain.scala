import java.nio.file.FileSystems
import java.util.ArrayList

def directorySeparator: String =
  import scala.language.unsafeNulls
  FileSystems.getDefault().getSeparator()

def getFirstOfFirst(xs: ArrayList[ArrayList[ArrayList[String]]]): String =
  import scala.language.unsafeNulls
  xs.get(0).get(0).get(0)