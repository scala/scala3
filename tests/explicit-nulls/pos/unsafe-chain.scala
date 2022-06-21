import java.nio.file.FileSystems
import java.util.ArrayList

class A:

  def directorySeparator: String =
    import scala.language.unsafeNulls
    FileSystems.getDefault().getSeparator()

  def getFirstOfFirst(xs: ArrayList[ArrayList[ArrayList[String]]]): String =
    import scala.language.unsafeNulls
    xs.get(0).get(0).get(0)

class B:
  import scala.language.unsafeNulls

  def directorySeparator: String =
    FileSystems.getDefault().getSeparator()

  def getFirstOfFirst(xs: ArrayList[ArrayList[ArrayList[String]]]): String =
    xs.get(0).get(0).get(0)