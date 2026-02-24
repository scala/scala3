import java.io.*
import java.nio.file.*

abstract class Using[Source, A]:
  def apply[R](src: Source)(f: A => R): R = ???

object Using:
  val fileInputStream: Using[Path, InputStream] = ???

def transfer(in: Path, out: OutputStream): Unit =
  Using.fileInputStream(in)(in => transfer(in, out))

def transfer(in: InputStream, out: OutputStream): Unit = ???