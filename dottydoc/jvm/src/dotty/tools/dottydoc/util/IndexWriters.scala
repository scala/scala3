package dotty.tools.dottydoc
package util

object IndexWriters {
  import html.EntityPage
  import model.Package
  import model.pickling._
  import prickle._

  def writeJs(packs: Map[String, Package], outPath: String): Unit = {
    for (pack <- packs.values) {
      println(s"""Writing '${pack.path.mkString(".")}'""")
      writeFile(
        EntityPage(pack, packs).render,
        outPath + pack.path.dropRight(1).mkString("/", "/", "/"),
        s"${pack.name.split("\\.").last}.html")
        //s"index.html")

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

    val pickled = Pickle.intoString(packs)
    val js = "UnparsedIndex = {}; UnparsedIndex.packages = " + pickled + ";"
    println("Writing index.js...")
    writeFile(js, outPath + "/../", "index.js")
    println("Done writing static material, building js-app")

  }

  def writeFile(str: String, path: String, file: String): Unit = {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }

    new java.io.File(path).mkdirs()
    printToFile(new java.io.File(path + file))(_.println(str))
  }
}
