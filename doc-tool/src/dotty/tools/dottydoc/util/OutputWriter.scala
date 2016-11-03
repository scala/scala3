package dotty.tools.dottydoc
package util

import dotty.tools.dotc.config.Printers.dottydoc

import _root_.java.io.{
  File => JFile,
  PrintWriter => JPrintWriter,
  FileReader => JFileReader,
  BufferedInputStream,
  InputStream,
  InputStreamReader,
  FileOutputStream,
  BufferedOutputStream,
  FileNotFoundException
}
import _root_.java.net.URL
import _root_.java.util.{ Map => JMap, List => JList }
import model.{ Entity, Package }
import model.json._
import com.github.mustachejava.DefaultMustacheFactory
import scala.collection.JavaConverters._

class OutputWriter {

  def writeJava(packs: JMap[String, Package], outPath: String, template: URL, resources: JList[URL]): Unit = {
    write(packs.asScala, outPath, template, resources.asScala)
  }

  def write(packs: collection.Map[String, Package], outPath: String, template: URL, resources: Traversable[URL]): Unit = {
    // Write all packages to `outPath`
    for (pack <- packs.values) {
      println(s"""Writing '${pack.path.mkString(".")}'""")
      writeFile(
        expandTemplate(template, pack, outPath),
        outPath + pack.path.mkString("/", "/", "/"),
        "index.html")

      // Write all package children to outPath
      for {
        child <- pack.children
        if child.kind != "package"
      } {
        println(s"""Writing '${child.path.mkString(".")}'""")
        writeFile(
          expandTemplate(template, child, outPath),
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

    // TODO: splitting the URL by '/' and taking the last means that we don't
    // allow folders among the resources
    resources.foreach(url => copy(url.openStream, outPath, url.getFile.split("/").last))

    println("Done writing static material, building js-app")
  }

  def writeJsonJava(index: JMap[String, Package], outputDir: String): Unit =
    writeJson(index.asScala, outputDir)

  def writeJson(index: collection.Map[String, Package], outputDir: String): Unit =
    writeFile(index.json, outputDir + "/", "index.json")

  def expandTemplate(template: URL, entity: Entity, outPath: String): String = try {
    import model.json._
    import model.java._

    val inputStream = template.openStream
    val writer = new _root_.java.io.StringWriter()
    val mf     = new DefaultMustacheFactory()

    def toRoot = "../" * (entity.path.length - { if (entity.isInstanceOf[Package]) 0 else 1 })

    val entityWithExtras = entity.asJava(Map(
      "assets" -> s"${toRoot}docassets",
      "index"  -> s"${toRoot}docassets/index.js",
      "currentEntity" -> entity.json
    ))

    mf.compile(new InputStreamReader(inputStream), "template")
      .execute(writer, entityWithExtras)

    inputStream.close()
    writer.flush()
    writer.toString
  } catch {
    case fnf: FileNotFoundException =>
      dottydoc.println(s"""Couldn't find the template: "${template.getFile}"...exiting""")
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

  def copy(src: InputStream, path: String, name: String): Unit = {
    val reader = new BufferedInputStream(src)
    try {
      val bytes = Stream.continually(reader.read).takeWhile(-1 != _).map(_.toByte)
      writeFile(bytes.toArray, path + "/docassets/", name)
      src.close()
    } finally reader.close()
  }
}
