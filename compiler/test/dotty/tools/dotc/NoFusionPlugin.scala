package dotty.tools.dotc

import core.Contexts.Context
import core.Phases.Phase
import plugins.ResearchPlugin

/** Gives every phase of the plan a group of its own, so that no miniphases are fused into a MegaPhase. */
class NoFusionPlugin extends ResearchPlugin:
  val name = "noFusion"
  override val description = "run each miniphase in a traversal of its own"

  def init(options: List[String], plan: List[List[Phase]])(using Context): List[List[Phase]] =
    plan.flatMap(_.map(List(_)))

object NoFusionPlugin:
  // `-Xplugin` for a directory holding just `plugin.properties`; the class loads from the test classpath.
  lazy val option: String =
    val dir = java.nio.file.Files.createTempDirectory("noFusionPlugin")
    java.nio.file.Files.writeString(dir.resolve("plugin.properties"), s"pluginClass=${classOf[NoFusionPlugin].getName}\n")
    dir.toFile.deleteOnExit()
    s"-Xplugin:$dir"
