package dotty.tools.vulpix

import java.io.{BufferedReader, File, InputStreamReader}
import java.net.{URL, URLClassLoader}
import java.util.ArrayList

object ChildJVMMain:
  val MessageStart = "##THIS IS THE START FOR ME, HELLO##"
  val MessageEnd = "##THIS IS THE END FOR ME, GOODBYE##"

  def runMain(dir: String): Unit =
    def meth =
      val jcp = System.getProperty("java.class.path")
      val sep = File.pathSeparator
      System.setProperty("java.class.path", if jcp == null then dir else dir + sep + jcp)

      val loader =
        val cp = ArrayList[URL]()
        val paths = dir.split(sep)
        for path <- paths do
          cp.add(File(path).toURI().toURL())
        val urls = cp.toArray(Array.ofDim[URL](cp.size))
        URLClassLoader(urls)

      val cls = loader.loadClass("Test")
      cls.getMethod("main", classOf[Array[String]])
    end meth
    val m =
      try meth
      catch t =>
        // Include the failure stack trace to the test output
        System.out.println(MessageStart)
        t.printStackTrace()
        throw t
    System.out.println(MessageStart)
    m.invoke(null, Array.empty[String])

  def main(args: Array[String]): Unit =
    inline def savingSystem[T](inline body: => T): T =
      val savedIn  = System.in
      val savedOut = System.out
      val savedErr = System.err
      try body
      finally
        System.setIn(savedIn)
        System.setOut(savedOut)
        System.setErr(savedErr)
    val stdin = BufferedReader(InputStreamReader(System.in))
    while true do
      savingSystem:
        runMain(stdin.readLine())
      println(MessageEnd)
