package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.DokkaSourceSetImpl
import java.io.File
import java.util.{ List => JList, Map => JMap}
import collection.JavaConverters._
import dotty.tastydoc.representations._

case class DottyDokkaConfig(compilationUnit: DDUnit) extends DokkaConfiguration:
  override def getOutputDir: String = new File("output").getAbsolutePath
  override def getCacheRoot: String = null
  override def getOfflineMode: Boolean = false
  override def getFailOnWarning: Boolean = false
  override def getSourceSets: JList[DokkaConfiguration.DokkaSourceSet] = List(mkSourceSet).asJava
  override def getModules: JList[DokkaConfiguration.DokkaModuleDescription] = List().asJava
  override def getPluginsClasspath: JList[File] = Nil.asJava
  override def getPluginsConfiguration: JMap[String, String] = Map("dotty.dokka.DottyDokkaPlugin" -> "dottydoc").asJava

  def mkSourceSet: DokkaConfiguration.DokkaSourceSet = new DokkaSourceSetImpl(
    "main",
    "Main",
    new DokkaSourceSetID("main", "main"),
    Nil.asJava,
    Nil.asJava,
    Set().asJava,
    Nil.asJava,
    List("output/BaseDocs.md").asJava,
    true,
    true,
    true,
    true,
    true,
    8,
    Nil.asJava,
    Nil.asJava,
    Nil.asJava,
    null,
    null,
    true,
    true,
    Nil.asJava,
    Platform.jvm
  ).asInstanceOf[DokkaConfiguration.DokkaSourceSet] // Why I do need to cast here? Kotlin magic?


object FakeDottyDokkaModule extends DokkaConfiguration.DokkaModuleDescription:
  override def getDocFile(): String = "docs.doc"
  override def getName() = "main"
  override def getPath() = "."