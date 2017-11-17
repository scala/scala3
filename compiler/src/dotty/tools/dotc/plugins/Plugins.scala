package dotty.tools.dotc
package plugins

import core._
import Contexts._
import config.PathResolver
import dotty.tools.io._
import Phases._
import config.Printers.plugins.{ println => debug }

import scala.collection.mutable.ListBuffer

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
    // plugin-specific options.
    // The user writes `-P:plugname:opt1,opt2`, but the plugin sees `List(opt1, opt2)`.
    def options(plugin: Plugin): List[String] = {
      def namec = plugin.name + ":"
      ctx.settings.pluginOptions.value filter (_ startsWith namec) map (_ stripPrefix namec)
    }

    // schedule plugins according to ordering constraints
    val pluginPhases = plugins.filter(!_.research).flatMap(plug => plug.init(options(plug)))
    val updatedPlan = Plugins.schedule(plan, pluginPhases)

    // add research plugins
    plugins.filter(_.research).foldRight(updatedPlan) { (plug, plan) => plug.init(options(plug), plan) }
  }
}

object Plugins {
  /** Insert plugin phases in the right place of the phase plan
   *
   *  The scheduling makes sure the ordering constraints of plugin phases are satisfied.
   *  If the ordering constraints are unsatisfiable, an exception is thrown.
   *
   *  Note: this algorithm is factored out for unit test.
   */
  def schedule(plan: List[List[Phase]], pluginPhases: List[PluginPhase]): List[List[Phase]] = {
    import scala.collection.mutable.{ Map => MMap, Set => MSet }
    type OrderingReq = (Set[Class[_]], Set[Class[_]])

    val orderRequirements = MMap[Class[_], OrderingReq]()
    val primitivePhases   = plan.flatMap(ps => ps.map(_.getClass.asInstanceOf[Class[_]])).toSet

    def isPrimitive(phase: Class[_]): Boolean = primitivePhases.contains(phase)

    def constraintConflict(phase: Phase): String = {
      val (runsAfter, runsBefore) = orderRequirements(phase.getClass)
      s"""
         |Ordering conflict for phase ${phase.phaseName}
         |after: ${runsAfter.mkString("[", ", ", "]")}
         |before: ${runsBefore.mkString("[", ", ", "]")}
       """.stripMargin
    }

    // init ordering map, no propagation
    pluginPhases.foreach { phase =>
      val runsAfter  : Set[Class[_]] = phase.runsAfter.asInstanceOf[Set[Class[_]]]
      val runsBefore : Set[Class[_]] = phase.runsBefore.asInstanceOf[Set[Class[_]]]

      orderRequirements.update(phase.getClass, (runsAfter, runsBefore))
    }

    // propagate ordering constraint : reflexivity
    pluginPhases.foreach { phase =>

      var (runsAfter, runsBefore) = orderRequirements(phase.getClass)

      // propagate transitive constraints to related phases
      runsAfter.filter(!isPrimitive(_)).foreach { phaseClass =>
        val (runsAfter1, runsBefore1) = orderRequirements(phaseClass)
        orderRequirements.update(phaseClass, (runsAfter1, runsBefore1 + phase.getClass))
      }

      runsBefore.filter(!isPrimitive(_)).foreach { phaseClass =>
        val (runsAfter1, runsBefore1) = orderRequirements(phaseClass)
        orderRequirements.update(phaseClass, (runsAfter1 + phase.getClass, runsBefore1))
      }

    }

    debug(
      s""" reflexive constraints:
         | ${orderRequirements.mkString("\n")}
       """.stripMargin
    )

    // propagate constraints from related phases to current phase: transitivity
    def propagate(phase: Phase): OrderingReq = {
      def propagateRunsBefore(beforePhase: Class[_]): Set[Class[_]] =
        if (beforePhase == phase.getClass)
          throw new Exception(constraintConflict(phase))
        else if (primitivePhases.contains(beforePhase))
          Set(beforePhase)
        else {
          val (_, runsBefore) = orderRequirements(beforePhase)
          runsBefore.flatMap(propagateRunsBefore) + beforePhase
        }

      def propagateRunsAfter(afterPhase: Class[_]): Set[Class[_]] =
        if (afterPhase == phase.getClass)
          throw new Exception(constraintConflict(phase))
        else if (primitivePhases.contains(afterPhase))
          Set(afterPhase)
        else {
          val (runsAfter, _) = orderRequirements(afterPhase)
          runsAfter.flatMap(propagateRunsAfter) + afterPhase
        }

      var (runsAfter, runsBefore) = orderRequirements(phase.getClass)

      runsAfter  = runsAfter.flatMap(propagateRunsAfter)
      runsBefore = runsBefore.flatMap(propagateRunsBefore)

      (runsAfter, runsBefore)
    }

    var updatedPlan = plan
    var insertedPhase = primitivePhases
    pluginPhases.sortBy(_.phaseName).foreach { phase =>
      var (runsAfter1, runsBefore1) = propagate(phase)

      debug(
        s"""propagated constraints for ${phase}:
           |after: ${runsAfter1.mkString("[", ", ", "]")}
           |before: ${runsBefore1.mkString("[", ", ", "]")}
         """.stripMargin
      )

      var runsAfter  = runsAfter1 & insertedPhase
      val runsBefore = runsBefore1 & insertedPhase

      // beforeReq met after the split
      val (before, after) = updatedPlan.span { ps =>
        val classes = ps.map(_.getClass)
        val runsAfterSat = runsAfter.isEmpty
        runsAfter = runsAfter -- classes
        // Prefer the point immediately before the first beforePhases.
        // If beforePhases not specified, insert at the point immediately
        // after the last afterPhases.
        !classes.exists(runsBefore.contains) &&
          !(runsBefore.isEmpty && runsAfterSat)
      }

      // check afterReq
      // error can occur if: a < b, b < c, c < a
      after.foreach { ps =>
        val classes = ps.map(_.getClass)
        if (classes.exists(runsAfter)) // afterReq satisfied
          throw new Exception(s"Ordering conflict for phase ${phase.phaseName}")
      }

      insertedPhase = insertedPhase + phase.getClass
      updatedPlan = before ++ (List(phase) :: after)
    }

    updatedPlan
  }
}
