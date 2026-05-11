import scala.language.experimental.safe
import scala.sys.process.*

val result = Seq("sh", "-c", "echo 'oh no'").!! // error

