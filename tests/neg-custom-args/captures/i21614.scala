import language.experimental.captureChecking
import caps.Capability
import caps.use

trait List[+T]:
  def map[U](f: T => U): List[U]

trait File extends Capability
class Logger(f: File^) extends Capability // <- will work if we remove the extends clause

def mkLoggers1[F <: File^](files: List[F]): List[Logger^] =
  files.map((f: F) => new Logger(f)) // error, Q: can we make this pass (see #19076)?

def mkLoggers2[C^](files: List[File^{C}]): List[Logger^] =
  files.map(new Logger(_)) // error, Q: can we improve the error message?
