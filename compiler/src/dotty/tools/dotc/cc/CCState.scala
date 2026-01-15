package dotty.tools
package dotc
package cc

import core.*
import CaptureSet.VarState
import collection.mutable
import reporting.Message
import Contexts.Context
import Types.MethodType
import Symbols.Symbol

/** Capture checking state, which is known to other capture checking components */
class CCState:
  import CCState.*

  // ------ Error diagnostics -----------------------------

  /** Warnings relating to upper approximations of capture sets with
   *  existentially bound variables.
   */
  val approxWarnings: mutable.ListBuffer[Message] = mutable.ListBuffer()

  // ------ BiTypeMap adjustment -----------------------

  private var myMapVars = true

  /** When mapping a capture set variable with a BiTypeMap, should we create a BiMapped set
   *  so that future elements can also be mapped, and elements added to the BiMapped
   *  are back-propagated? Or should we return the capture set as is? Turned off when
   *  creating capture set variables for the first time, since we then do not want to
   *  change the binder to the original type without capture sets when back propagating.
   *  Error cases where this shows: pos-customargs/captures/lists.scala, method m2c, and
   *  pos-customargs/captures/infer-exists.scala,
   */
  def mapVars(using Context) = myMapVars

  /** Don't map capset variables with BiTypeMaps during this `op` */
  inline def withNoVarsMapped[T](op: => T)(using Context): T =
    val saved = mapVars
    myMapVars = false
    try op finally myMapVars = saved

  // ------ Iteration count of capture checking run

  private var iterCount = 0

  def iterationId = iterCount

  def nextIteration[T](op: => T): T =
    iterCount += 1
    try op finally iterCount -= 1

  def start(): Unit =
    iterCount = 1

  private var mySepCheck = false

  /** Are we currently running separation checks? */
  def isSepCheck = mySepCheck

  def inSepCheck(op: => Unit): Unit =
    val saved = mySepCheck
    mySepCheck = true
    try op finally mySepCheck = saved

  // ------ Global counters -----------------------

  /** Next CaptureSet.Var id */
  var varId = 0

  /** Next root id */
  var rootId = 0

  // ------ VarState singleton objects ------------
  // See CaptureSet.VarState creation methods for documentation

  object Separate extends VarState.Separating
  object HardSeparate extends VarState.Separating
  object Unrecorded extends VarState.Unrecorded
  object ClosedUnrecorded extends VarState.ClosedUnrecorded

  // ----- Mirrors for local vars -------------------------

  /** A cache for mirrors of local mutable vars */
  val varMirrors = util.EqHashMap[Symbol, Symbol]()

  // ------ Context info accessed from companion object when isCaptureCheckingOrSetup is true

  private var openExistentialScopes: List[MethodType] = Nil

  private var globalCapIsRoot: Boolean = false

  private var collapseLocalCaps: Boolean = false

  private var discardUses: Boolean = false

object CCState:

  /** If we are currently in capture checking or setup, and `mt` is a method
   *  type that is not a prefix of a curried method, perform `op` assuming
   *  a fresh enclosing existential scope `mt`, otherwise perform `op` directly.
   */
  inline def inNewExistentialScope[T](mt: MethodType)(op: => T)(using Context): T =
    if isCaptureCheckingOrSetup then
      val ccs = ccState
      val saved = ccs.openExistentialScopes
      if mt.marksExistentialScope then ccs.openExistentialScopes = mt :: ccs.openExistentialScopes
      try op finally ccs.openExistentialScopes = saved
    else
      op

  /** The currently opened existential scopes */
  def openExistentialScopes(using Context): List[MethodType] = ccState.openExistentialScopes

  /** Run `op` under the assumption that `caps.any` can subsume all other capabilties
   *  except Result capabilities. Every use of this method should be scrutinized
   *  for whether it introduces an unsoundness hole.
   */
  inline def withGlobalCapAsRoot[T](op: => T)(using Context): T =
    if isCaptureCheckingOrSetup then
      val ccs = ccState
      val saved = ccs.globalCapIsRoot
      ccs.globalCapIsRoot = true
      try op finally ccs.globalCapIsRoot = saved
    else op

  /** Is `caps.any` a root capability that is allowed to subsume other capabilities? */
  def globalCapIsRoot(using Context): Boolean = ccState.globalCapIsRoot

  /** Run `op` under the assumption that all LocalCap instances are equal
   *  to each other and to GlobalAny.
   *  Needed to make override checking work for types containing LocalCaps.
   *  Asserted in override checking, tested in maxSubsumes.
   *  Is this sound? Test case is neg-custom-args/captures/leaked-curried.scala.
   */
  inline def withCollapsedLocalCaps[T](op: => T)(using Context): T =
    if isCaptureCheckingOrSetup then
      val ccs = ccState
      val saved = ccs.collapseLocalCaps
      ccs.collapseLocalCaps = true
      try op finally ccs.collapseLocalCaps = saved
    else op

  /** Should all LocalCap instances be treated as equal to GlobalAny? */
  def collapseLocalCaps(using Context): Boolean = ccState.collapseLocalCaps

  /** Run `op` but suppress all recording of uses in `markFree` */
   inline def withDiscardedUses[T](op: => T)(using Context): T =
    if isCaptureCheckingOrSetup then
      val ccs = ccState
      val saved = ccs.discardUses
      ccs.discardUses = true
      try op finally ccs.discardUses = saved
    else op

  /** Should uses not be recorded in markFree? */
  def discardUses(using Context): Boolean = ccState.discardUses

end CCState
