package dotty.tools.dottydoc
package util

import dotty.tools.dotc.config.Printers.dottydoc

import _root_.java.io.{
  File => JFile,
  PrintWriter => JPrintWriter,
  FileReader => JFileReader,
  BufferedInputStream,
  FileInputStream,
  FileOutputStream,
  BufferedOutputStream,
  FileNotFoundException
}
import _root_.java.util.{ Map => JMap, List => JList }
import model.{ Entity, Package }
import model.json._
import com.github.mustachejava.DefaultMustacheFactory


class OutputWriter {

  def writeJava(packs: JMap[String, Package], templatePath: String, outPath: String, resources: JList[String]): Unit = {
    import scala.collection.JavaConverters._
    write(packs.asScala, templatePath, outPath, resources.asScala)
  }

  def write(packs: collection.Map[String, Package], templatePath: String, outPath: String, resources: Iterable[String]): Unit = {
    // Write all packages to `outPath`
    for (pack <- packs.values) {
      println(s"""Writing '${pack.path.mkString(".")}'""")
      writeFile(
        expandTemplate(templatePath, pack, outPath),
        outPath + pack.path.mkString("/", "/", "/"),
        "index.html")

      // Write all package children to outPath
      for {
        child <- pack.children
        if child.kind != "package"
      } {
        println(s"""Writing '${child.path.mkString(".")}'""")
        writeFile(
          expandTemplate(templatePath, child, outPath),
          outPath + child.path.dropRight(1).mkString("/", "/", "/"),
          child.path.last + ".html")
      }
    }

    // Write full index to outPath
    val js = "Index = {}; Index.packages = " + packs.json + ";"
    println("Writing index.js...")
    writeFile(js, outPath + "/docassets/", "index.js")

    // Write resources to outPath
    println("Copying CSS/JS resources to destination...")
    assert(resources.nonEmpty)
    resources.map(s => copy(new JFile(s), outPath))

    println("Done writing static material, building js-app")
  }

  def expandTemplate(templatePath: String, entity: Entity, outPath: String): String = try {
    import model.java._
    import scala.collection.JavaConverters._
    val writer = new _root_.java.io.StringWriter()
    val mf     = new DefaultMustacheFactory()

    def toRoot = "../" * (entity.path.length - 1)

    val entityWithExtras = entity.asJava(Map(
      "assets" -> s"${toRoot}docassets",
      "index"  -> s"${toRoot}docassets/index.js"
    ))

    mf.compile(new JFileReader(templatePath), "template")
      .execute(writer, entityWithExtras)

    writer.flush()
    writer.toString
  } catch {
    case fnf: FileNotFoundException =>
      dottydoc.println(s"""Couldn't find the template: "$templatePath"...exiting""")
      System.exit(1); ""
  }

  def writeFile(str: String, path: String, file: String): Unit =
    writeFile(str.map(_.toByte).toArray, path, file)

  def writeFile(bytes: Array[Byte], path: String, file: String): Unit = {
    def printToFile(f: JFile)(op: JPrintWriter => Unit) = {
      val bos = new BufferedOutputStream(new FileOutputStream(f))
      try {
        Stream.continually(bos.write(bytes))
      } finally bos.close()
    }

    new JFile(path).mkdirs()
    printToFile(new JFile(path + file))(printer => bytes.foreach(printer.print))
  }

  def copy(src: JFile, path: String): Unit = {
    val reader = new BufferedInputStream(new FileInputStream(src))
    try {
      val bytes  = Stream.continually(reader.read).takeWhile(-1 != _).map(_.toByte)
      writeFile(bytes.toArray, path + "/docassets/", src.getName)
    } finally reader.close()
  }
}
