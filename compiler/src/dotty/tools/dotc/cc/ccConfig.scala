package dotty.tools
package dotc
package cc

import core.Contexts.Context
import config.{Feature, SourceVersion}

object ccConfig:

  /** If enabled, use a special path in recheckClosure for closures
   *  to compare the result tpt of the anonymous functon with the expected
   *  result type. This can narrow the scope of error messages.
   */
  inline val preTypeClosureResults = false

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

  /** If true, turn on separation checking */
  def useSepChecks(using Context): Boolean =
    Feature.sourceVersion.stable.isAtLeast(SourceVersion.`3.7`)

  /** Not used currently. Handy for trying out new features */
  def newScheme(using Context): Boolean =
    Feature.sourceVersion.stable.isAtLeast(SourceVersion.`3.8`)

end ccConfig