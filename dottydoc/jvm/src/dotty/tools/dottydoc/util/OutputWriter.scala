package dotty.tools.dottydoc
package util

import java.io.{File => JFile, BufferedInputStream, FileInputStream, FileOutputStream, BufferedOutputStream}
import html.EntityPage
import model.Package
import spray.json._
import model.json._

class OutputWriter {
  def write(packs: Map[String, Package], outPath: String): Unit = {
    // Write all packages to `outPath`
    for (pack <- packs.values) {
      println(s"""Writing '${pack.path.mkString(".")}'""")
      writeFile(
        EntityPage(pack, packs).render,
        outPath + pack.path.mkString("/", "/", "/"),
        "index.html")

      // Write all package children to outPath
      for {
        child <- pack.children
        if child.kind != "package"
      } {
        println(s"""Writing '${child.path.mkString(".")}'""")
        writeFile(
          EntityPage(child, packs).render,
          outPath + child.path.dropRight(1).mkString("/", "/", "/"),
          child.path.last + ".html")
      }
    }

    // Write full index to outPath
    //val pickled = Pickle.intoString(packs)
    val pickled = packs.toJson
    val js = "UnparsedIndex = {}; UnparsedIndex.packages = " + pickled + ";"
    println("Writing index.js...")
    writeFile(js, outPath + "/", "index.js")

    // Write resources to outPath
    println("Copying CSS/JS resources to destination...")
    assert(resources.nonEmpty)
    resources.map(copy(_, outPath))

    println("Done writing static material, building js-app")
  }

  def writeFile(str: String, path: String, file: String): Unit =
    writeFile(str.map(_.toByte).toArray, path, file)

  def writeFile(bytes: Array[Byte], path: String, file: String): Unit = {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
      val bos = new BufferedOutputStream(new FileOutputStream(f))
      try {
        Stream.continually(bos.write(bytes))
      } finally bos.close()
    }

    new java.io.File(path).mkdirs()
    printToFile(new java.io.File(path + file))(printer => bytes.foreach(printer.print))
  }

  def copy(src: JFile, path: String): Unit = {
    val reader = new BufferedInputStream(new FileInputStream(src))
    try {
      val bytes  = Stream.continually(reader.read).takeWhile(-1 != _).map(_.toByte)
      writeFile(bytes.toArray, path + "/static/", src.getName)
    } finally reader.close()
  }

  /** All static resources */
  private val resources: Iterable[JFile] = List(
    "/MaterialIcons-Regular.eot",
    "/MaterialIcons-Regular.ijmap",
    "/MaterialIcons-Regular.svg",
    "/MaterialIcons-Regular.ttf",
    "/MaterialIcons-Regular.woff",
    "/MaterialIcons-Regular.woff2",
    "/codepoints",
    "/github.css",
    "/highlight.pack.js",
    "/index.css",
    "/material-icons.css",
    "/material.min.css",
    "/material.min.js",
    "/dottydoc-fastopt.js"
  ).map { f =>
    new JFile(this.getClass.getResource(f).toURI)
  }
}
