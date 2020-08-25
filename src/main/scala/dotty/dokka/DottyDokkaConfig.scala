package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.DokkaSourceSetImpl
import java.io.File
import java.util.{ List => JList, Map => JMap}
import collection.JavaConverters._

case class DottyDokkaConfig(docConfiguration: DocConfiguration) extends DokkaConfiguration:
  override def getOutputDir: String = docConfiguration.args.output.getAbsolutePath
  override def getCacheRoot: String = null
  override def getOfflineMode: Boolean = false
  override def getFailOnWarning: Boolean = false
  override def getSourceSets: JList[DokkaConfiguration.DokkaSourceSet] = List(mkSourceSet).asJava
  override def getModules: JList[DokkaConfiguration.DokkaModuleDescription] = List().asJava
  override def getPluginsClasspath: JList[File] = Nil.asJava

  override def getPluginsConfiguration: JMap[String, String] = Map(
    "dotty.dokka.DottyDokkaPlugin" -> "dottydoc",
    "ExternalDocsTooKey" -> docConfiguration.args.docsRoot.orNull
    ).asJava

  def mkSourceSet: DokkaConfiguration.DokkaSourceSet = new DokkaSourceSetImpl(
    docConfiguration.args.name,
    "Main",
    new DokkaSourceSetID(docConfiguration.args.name, "main"),
    Nil.asJava,
    Nil.asJava,
    Set().asJava,
    Nil.asJava,
    Nil.asJava,
    true,
    /*includeRootPackage*/ false,
    false, /* changed because of exception in reportUndocumentedTransformer - there's 'when' which doesnt match because it contains only KotlinVisbility cases */
    true,
    true,
    8,
    docConfiguration.args.sourceLinks.map(SourceLinkDefinitionImpl.Companion.parseSourceLinkDefinition(_)).asJava,
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
