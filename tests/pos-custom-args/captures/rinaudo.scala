import language.experimental.captureChecking
import caps.*

trait FileSystem extends SharedCapability:
  def print(msg: String): Unit

class Logger(using fs: FileSystem):
    def info(msg: String): Unit = fs.print(msg)

def log(msg: String): FileSystem ?-> Unit =
  val l = new Logger
  l.info(msg)