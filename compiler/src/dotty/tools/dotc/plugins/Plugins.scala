package dotty.tools.dotc
package plugins

import core._
import Contexts._
import config.PathResolver
import dotty.tools.io._
import Phases._

/** Support for run-time loading of compiler plugins.
 *
 *  @author Lex Spoon
 *  @version 1.1, 2009/1/2
 *  Updated 2009/1/2 by Anders Bach Nielsen: Added features to implement SIP 00002
 */
trait Plugins {
  self: Context =>

  /** Load a rough list of the plugins.  For speed, it
   *  does not instantiate a compiler run.  Therefore it cannot
   *  test for same-named phases or other problems that are
   *  filtered from the final list of plugins.
   */
  protected def loadRoughPluginsList(implicit ctx: Context): List[Plugin] = {
    def asPath(p: String) = ClassPath split p
    val paths  = ctx.settings.plugin.value filter (_ != "") map (s => asPath(s) map Path.apply)
    val dirs   = {
      def injectDefault(s: String) = if (s.isEmpty) PathResolver.Defaults.scalaPluginPath else s
      asPath(ctx.settings.pluginsDir.value) map injectDefault map Path.apply
    }
    val maybes = Plugin.loadAllFrom(paths, dirs, ctx.settings.disable.value)
    val (goods, errors) = maybes partition (_.isSuccess)
    // Explicit parameterization of recover to avoid -Xlint warning about inferred Any
    errors foreach (_.recover[Any] {
      // legacy behavior ignores altogether, so at least warn devs
      case e: MissingPluginException => warning(e.getMessage)
      case e: Exception              => inform(e.getMessage)
    })
    val classes = goods map (_.get)  // flatten

    // Each plugin must only be instantiated once. A common pattern
    // is to register annotation checkers during object construction, so
    // creating multiple plugin instances will leave behind stale checkers.
    classes map (Plugin.instantiate(_))
  }

  private var _roughPluginsList: List[Plugin] = _
  protected def roughPluginsList(implicit ctx: Context): List[Plugin] =
    if (_roughPluginsList == null) {
      _roughPluginsList = loadRoughPluginsList
      _roughPluginsList
    }
    else _roughPluginsList

  /** Load all available plugins.  Skips plugins that
   *  either have the same name as another one, or which
   *  define a phase name that another one does.
   */
  protected def loadPlugins(implicit ctx: Context): List[Plugin] = {
    // remove any with conflicting names or subcomponent names
    def pick(
      plugins: List[Plugin],
      plugNames: Set[String]): List[Plugin] =
    {
      if (plugins.isEmpty) return Nil // early return

      val plug :: tail      = plugins
      def withoutPlug       = pick(tail, plugNames)
      def withPlug          = plug :: pick(tail, plugNames + plug.name)

      def note(msg: String): Unit = if (ctx.settings.verbose.value) inform(msg format plug.name)
      def fail(msg: String)       = { note(msg) ; withoutPlug }

      if (plugNames contains plug.name)
        fail("[skipping a repeated plugin: %s]")
      else if (ctx.settings.disable.value contains plug.name)
        fail("[disabling plugin: %s]")
      else {
        note("[loaded plugin %s]")
        withPlug
      }
    }

    val plugs = pick(roughPluginsList, ctx.phasePlan.flatten.map(_.phaseName).toSet)

    // Verify required plugins are present.
    for (req <- ctx.settings.require.value ; if !(plugs exists (_.name == req)))
      ctx.error("Missing required plugin: " + req)

    // Verify no non-existent plugin given with -P
    for {
      opt <- ctx.settings.pluginOptions.value
      if !(plugs exists (opt startsWith _.name + ":"))
    } ctx.error("bad option: -P:" + opt)

    plugs
  }

  private var _plugins: List[Plugin] = _
  def plugins(implicit ctx: Context): List[Plugin] =
  if (_plugins == null) {
    _plugins = loadPlugins
    _plugins
  }
  else _plugins


  /** A description of all the plugins that are loaded */
  def pluginDescriptions: String =
    roughPluginsList map (x => "%s - %s".format(x.name, x.description)) mkString "\n"

  /** Summary of the options for all loaded plugins */
  def pluginOptionsHelp: String =
    (for (plug <- roughPluginsList ; help <- plug.optionsHelp) yield {
      "\nOptions for plugin '%s':\n%s\n".format(plug.name, help)
    }).mkString

  /** Add plugin phases to phase plan */
  def addPluginPhases(plan: List[List[Phase]])(implicit ctx: Context): List[List[Phase]] = {
    import scala.collection.mutable.{ Set => MSet, Map => MMap }
    type OrderingReq = (MSet[Class[_]], MSet[Class[_]])

    val orderRequirements = MMap[Class[_], OrderingReq]()

    def updateOrdering(phase: PluginPhase): Unit = {
      val runsBefore: MSet[Class[_]] = MSet(phase.runsBefore.toSeq: _*)
      val runsAfter: MSet[Class[_]]  = MSet(phase.runsAfter.toSeq: _*)

      if (!orderRequirements.contains(phase.getClass)) {
        orderRequirements.update(phase.getClass, (runsBefore, runsAfter) )
      }

      runsBefore.foreach { phaseClass =>
        if (!orderRequirements.contains(phaseClass))
          orderRequirements.update(phaseClass, (MSet.empty, MSet.empty))
        val (_, runsAfter) = orderRequirements(phaseClass)
        runsAfter += phase.getClass
      }

      runsAfter.foreach { phaseClass =>
        if (!orderRequirements.contains(phaseClass))
          orderRequirements.update(phaseClass, (MSet.empty, MSet.empty))
        val (runsBefore, _) = orderRequirements(phaseClass)
        runsBefore += phase.getClass
      }
    }

    // add non-research plugins
    var updatedPlan = plan
    plugins.filter(!_.research).foreach { plug =>
      plug.components.foreach { phase =>
        updateOrdering(phase)

        val beforePhases: MSet[Class[_]] = MSet(phase.runsBefore.toSeq: _*)
        val afterPhases: MSet[Class[_]]  = MSet(phase.runsAfter.toSeq: _*)

        val (before, after) = updatedPlan.span { ps =>
          val classes = ps.map(_.getClass)
          afterPhases --= classes
          !classes.exists(beforePhases.contains)  // beforeReq satisfied
        }

        // check afterReq
        // error can occur if: a < b, b < c, c < a
        after.foreach { ps =>
          val classes = ps.map(_.getClass)
          if (classes.exists(afterPhases))  // afterReq satisfied
            throw new Exception(s"Ordering conflict for plugin ${plug.name}")
        }

        updatedPlan = before ++ (List(phase) :: after)
      }
    }

    // add research plugins
    ctx.plugins.filter(_.research).foldRight(updatedPlan) { (plug, plan) => plug.init(plan) }
  }
}
