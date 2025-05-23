package dotty.tools
package dotc
package cc

import core.*
import CaptureSet.{CompareResult, VarState}
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

  // ------ Level handling ---------------------------

  private var curLevel: Level = outermostLevel

  /** The level of the current environment. Levels start at 0 and increase for
   *  each nested function or class. -1 means the level is undefined.
   */
  def currentLevel(using Context): Level = curLevel

  /** Perform `op` in the next inner level */
  inline def inNestedLevel[T](inline op: T)(using Context): T =
    val saved = curLevel
    curLevel = curLevel.nextInner
    try op finally curLevel = saved

  /** Perform `op` in the next inner level unless `p` holds. */
  inline def inNestedLevelUnless[T](inline p: Boolean)(inline op: T)(using Context): T =
    val saved = curLevel
    if !p then curLevel = curLevel.nextInner
    try op finally curLevel = saved

  /** A map recording the level of a symbol */
  private val mySymLevel: mutable.Map[Symbol, Level] = mutable.Map()

  def symLevel(sym: Symbol): Level = mySymLevel.getOrElse(sym, undefinedLevel)

  def recordLevel(sym: Symbol)(using Context): Unit = mySymLevel(sym) = curLevel

  // ------ BiTypeMap adjustment -----------------------

  private var myMapFutureElems = true

  /** When mapping a capture set with a BiTypeMap, should we create a BiMapped set
   *  so that future elements can also be mapped, and elements added to the BiMapped
   *  are back-propagated? Turned off when creating capture set variables for the
   *  first time, since we then do not want to change the binder to the original type
   *  without capture sets when back propagating. Error case where this shows:
   *  pos-customargs/captures/lists.scala, method m2c.
   */
  def mapFutureElems(using Context) = myMapFutureElems

  /** Don't map future elements in this `op` */
  inline def withoutMappedFutureElems[T](op: => T)(using Context): T =
    val saved = mapFutureElems
    myMapFutureElems = false
    try op finally myMapFutureElems = saved

  // ------ Iteration count of capture checking run

  private var iterCount = 1

  def iterationId = iterCount

  def nextIteration[T](op: => T): T =
    iterCount += 1
    try op finally iterCount -= 1

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

  // ------ Context info accessed from companion object when isCaptureCheckingOrSetup is true

  private var openExistentialScopes: List[MethodType] = Nil

  private var capIsRoot: Boolean = false

  private var collapseFresh: Boolean = false

object CCState:

  opaque type Level = Int

  val undefinedLevel: Level = -1

  val outermostLevel: Level = 0

  extension (x: Level)
    def isDefined: Boolean = x >= 0
    def <= (y: Level) = (x: Int) <= y
    def nextInner: Level = if isDefined then x + 1 else x

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

  /** Run `op` under the assumption that `cap` can subsume all other capabilties
   *  except Result capabilities. Every use of this method should be scrutinized
   *  for whether it introduces an unsoundness hole.
   */
  inline def withCapAsRoot[T](op: => T)(using Context): T =
    if isCaptureCheckingOrSetup then
      val ccs = ccState
      val saved = ccs.capIsRoot
      ccs.capIsRoot = true
      try op finally ccs.capIsRoot = saved
    else op

  /** Is `caps.cap` a root capability that is allowed to subsume other capabilities? */
  def capIsRoot(using Context): Boolean = ccState.capIsRoot

  /** Run `op` under the assumption that all FreshCap instances are equal
   *  to each other and to GlobalCap.
   *  Needed to make override checking of types containing fresh work.
   *  Asserted in override checking, tested in maxSubsumes.
   *  Is this sound? Test case is neg-custom-args/captures/leaked-curried.scala.
   */
  inline def withCollapsedFresh[T](op: => T)(using Context): T =
    if isCaptureCheckingOrSetup then
      val ccs = ccState
      val saved = ccs.collapseFresh
      ccs.collapseFresh = true
      try op finally ccs.collapseFresh = saved
    else op

  /** Should all FreshCap instances be treated as equal to GlobalCap? */
  def collapseFresh(using Context): Boolean = ccState.collapseFresh

end CCState
