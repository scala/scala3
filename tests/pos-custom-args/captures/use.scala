import language.experimental.captureChecking
import java.io.FileOutputStream
import java.io.Closeable
import java.io.OutputStream

def use1[R <: Closeable, T](r: R)(f: R^ => T): T =
  try f(r)
  finally r.close()

@main def main4 =
  use1(new FileOutputStream("data.txt"))(out => ())
