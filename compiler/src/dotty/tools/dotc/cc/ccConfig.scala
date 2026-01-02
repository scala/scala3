package dotty.tools
package dotc
package cc

import core.Contexts.Context
import config.{Feature, SourceVersion}

object ccConfig:

  /** If enabled, cache capture sets of infos capabilties */
  inline val cacheCaptureSetOfInfo = false

  /** If this and `preTypeClosureResults` are both enabled, disable `preTypeClosureResults`
   *  for eta expansions. This can improve some error messages.
   */
  inline val handleEtaExpansionsSpecially = true

  /** Don't require @use for reach capabilities that are accessed
   *  only in a nested closure. This is unsound without additional
   *  mitigation measures, as shown by unsound-reach-5.scala.
   */
  inline val deferredReaches = false

  /** Check that if a type map (which is not a BiTypeMap) maps initial capture
   *  set variable elements to themselves it will not map any elements added in
   *  the future to something else. That is, we can safely use a capture set
   *  variable itself as the image under the map. By default this is off since it
   *  is a bit expensive to check.
   */
  inline val checkSkippedMaps = false

  /** Always repeat a capture checking run at least once if there are no errors
   *  yet. Used for stress-testing the logic for when a new capture checking run needs
   *  to be scheduled because a provisionally solved capture set was later extended.
   *  So far this happens only in very few tests. With the flag on, the logic is
   *  tested for all tests except neg tests.
   */
  inline val alwaysRepeatRun = false

  /** After capture checking, check that no capture set contains ParamRefs that are outside
   *  its scope. This used to occur and was fixed by healTypeParam. It should no longer
   *  occur now.
   */
  inline val postCheckCapturesets = false

  /** If true take as the underlying capture set of a capability of function type
   *  the capture set along the span, including capture sets of function results.
   */
  inline val useSpanCapset = false

  /** If true, do level checking for FreshCap instances */
  def useFreshLevels(using Context): Boolean =
    Feature.sourceVersion.stable.isAtLeast(SourceVersion.`3.7`)

  /** Not used currently. Handy for trying out new features */
  def newScheme(using ctx: Context): Boolean =
    Feature.sourceVersion.stable.isAtLeast(SourceVersion.`3.8`)

  /** Allow @use annotations */
  def allowUse(using Context): Boolean =
    Feature.sourceVersion.stable.isAtMost(SourceVersion.`3.7`)

  /** Treat arrays as mutable types and force all mutable fields to be in Stateful
   *  classes, unless they are annotated with @untrackedCaptures.
   *  Enabled under separation checking
   */
  def strictMutability(using Context): Boolean =
    Feature.enabled(Feature.separationChecking)

end ccConfig
