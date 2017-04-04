package dotty.tools.dotc
package vulpix

import java.io.{
  File => JFile,
  InputStream, ObjectInputStream,
  OutputStream, ObjectOutputStream,
  ByteArrayOutputStream, PrintStream
}
import java.lang.reflect.InvocationTargetException

import dotty.tools.dotc.vulpix.Statuses._

object ChildMain {
  val realStdin = System.in
  val realStderr = System.err
  val realStdout = System.out

  private def runMain(dir: JFile): Status = {
    def renderStackTrace(ex: Throwable): String =
      ex.getStackTrace
        .takeWhile(_.getMethodName != "invoke0")
        .mkString("    ", "\n    ", "")

    def resetOutDescriptors(): Unit = {
      System.setOut(realStdout)
      System.setErr(realStderr)
    }

    import java.net.{ URL, URLClassLoader }

    val printStream = new ByteArrayOutputStream

    try {
      // Do classloading magic and running here:
      val ucl = new URLClassLoader(Array(dir.toURI.toURL))
      val cls = ucl.loadClass("Test")
      val meth = cls.getMethod("main", classOf[Array[String]])

      try {
        val ps = new PrintStream(printStream)
        System.setOut(ps)
        System.setErr(ps)
        Console.withOut(printStream) {
          Console.withErr(printStream) {
            // invoke main with "jvm" as arg
            meth.invoke(null, Array("jvm"))
          }
        }
        resetOutDescriptors()
      } catch {
        case t: Throwable =>
          resetOutDescriptors()
          throw t
      }
      new Success(printStream.toString("utf-8"))
    }
    catch {
      case ex: NoSuchMethodException =>
        val msg = s"test in '$dir' did not contain method: ${ex.getMessage}"
        new Failure(msg, renderStackTrace(ex.getCause))

      case ex: ClassNotFoundException =>
        val msg = s"test in '$dir' did not contain class: ${ex.getMessage}"
        new Failure(msg, renderStackTrace(ex.getCause))

      case ex: InvocationTargetException =>
        val msg = s"An exception ocurred when running main: ${ex.getCause}"
        new Failure(msg, renderStackTrace(ex.getCause))
    }
  }

  def main(args: Array[String]): Unit = {
    val stdin = new ObjectInputStream(System.in);
    val stdout = new ObjectOutputStream(System.out);

    while (true) {
      val dir = stdin.readObject().asInstanceOf[JFile]
      stdout.writeObject(runMain(dir))
      stdout.flush()
    }
  }
}
