import language.experimental.captureChecking
import caps.Capability
import caps.unbox

trait File extends Capability
class Logger(f: File^) extends Capability // <- will work if we remove the extends clause

def mkLoggers1[F <: File^](@unbox files: List[F]): List[Logger^] =
  files.map((f: F) => new Logger(f)) // error, Q: can we make this pass (see #19076)?

def mkLoggers2(@unbox files: List[File^]): List[Logger^] =
  files.map(new Logger(_)) // error, Q: can we improve the error message?
